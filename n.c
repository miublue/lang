#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define LENGTH(X) (sizeof(X)/sizeof((X)[0]))
#define ALLOC_SZ 1024
/* XXX: these should probably be dynamically allocated anyways */
#define MAX_FUNS 1024
#define MAX_STRINGS 1024
#define MAX_VARS 256
#define MAX_ARGS 6

enum { LVALUE_NONE, LVALUE_REF, LVALUE_DEREF };

enum {
    TK_EOF, TK_ID, TK_INT, TK_STR, TK_CHR,
    TK_IF, TK_ELSE, TK_WHILE, TK_BREAK,
    TK_FUN, TK_RETURN, TK_EXTERN, TK_INLINE,
    TK_LBRACK, TK_RBRACK, TK_LPAREN, TK_RPAREN,
    TK_LSQUARE, TK_RSQUARE, TK_COLON, TK_COMMA,
    TK_ADD, TK_SUB, TK_MUL, TK_DIV, TK_MOD,
    TK_BAND, TK_BOR, TK_BNOT, TK_EQ, TK_EQEQ, TK_NEQ,
    TK_LT, TK_GT, TK_LEQ, TK_GEQ, TK_SHL, TK_SHR,
    TK_AND, TK_OR, TK_AT, TK_NOT, TK_XOR,
    MAX_TOKENS
};

enum {
    PREC_BANDOR,
    PREC_COMPARE,
    PREC_ADDSUB,
    PREC_MULDIV,
    PREC_ANDOR,
    PREC_SHLSHR,
    MAX_PRECEDENCE
};

static const int PRECEDENCES[][MAX_PRECEDENCE] = {
    [PREC_BANDOR]  = { TK_BAND, TK_BOR },
    [PREC_COMPARE] = { TK_EQEQ, TK_NEQ, TK_LT, TK_GT, TK_LEQ, TK_GEQ },
    [PREC_ADDSUB]  = { TK_ADD, TK_SUB },
    [PREC_MULDIV]  = { TK_MUL, TK_DIV, TK_MOD },
    [PREC_ANDOR]   = { TK_AND, TK_XOR, TK_OR },
    [PREC_SHLSHR]  = { TK_SHL, TK_SHR },
};

static const char *ARGREGS[MAX_ARGS] = {
    "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9",
};

static const char *CHAROPS = "{}()[]:,+-*/%&|@!=<>~^";
static const char *OPERATORS[] = {
    "{", "}", "(", ")", "[", "]", ":", ",",
    "+", "-", "*", "/", "%", "&&", "||",
    "!", "=", "==", "!=", "<", ">", "<=",
    ">=", "<<", ">>", "&", "|", "@", "~", "^",
};

static const char *KEYWORDS[] = {
    "if", "else", "while", "break",
    "fun", "return", "extern", "inline",
};

/* XXX: these structures are literally the same */
typedef struct {
    uint32_t kind, sz;
    char *ptr;
} token_t;

typedef struct {
    int sz;
    char *name;
} type_t;

typedef struct {
    uint32_t sz;
    char *name;
    type_t type;
} var_t;

typedef struct {
    uint32_t arity, sz;
    type_t ret, args[MAX_ARGS];
    char *name;
} fun_t;

/* 8 byte aligned types */
static type_t types[ALLOC_SZ] = {
    (type_t) { 8, "char" },
    (type_t) { 8, "int" },
    (type_t) { 8, "ptr" },
    (type_t) { 8, "str" },
};
static uint32_t types_sz = LENGTH(types);
/* XXX: 'ptr' should keep track of which type it points to */

static const char *GENCMP[MAX_TOKENS] = {
    [TK_EQEQ] = "sete",
    [TK_NEQ]  = "setne",
    [TK_LT]   = "setl",
    [TK_LEQ]  = "setle",
    [TK_GT]   = "setg",
    [TK_GEQ]  = "setge",
};

static const char *GENOPS[MAX_TOKENS] = {
    [TK_SHL] =  "  mov %rax,%rcx\n"
                "  pop %rax\n"
                "  shl %cl,%rax\n",
    [TK_SHR] =  "  mov %rax,%rcx\n"
                "  pop %rax\n",
                "  sar %cl,%rax\n",
    [TK_AND] =  "  pop %rdi\n"
                "  and %rdi,%rax\n",
    [TK_XOR] =  "  pop %rdi\n"
                "  xor %rdi,%rax\n",
    [TK_OR]  =  "  pop %rdi\n"
                "  or %rdi,%rax\n",
    [TK_ADD] =  "  pop %rdi\n"
                "  add %rdi,%rax\n",
    [TK_SUB] =  "  mov %rax,%rdi\n"
                "  pop %rax\n"
                "  sub %rdi,%rax\n",
    [TK_MUL] =  "  pop %rdi\n"
                "  imul %rdi,%rax\n",
    [TK_DIV] =  "  mov %rax,%rdi\n"
                "  pop %rax\n"
                "  cqo\n"
                "  idiv %rdi\n",
    [TK_MOD] =  "  mov %rax,%rdi\n"
                "  pop %rax\n"
                "  cqo\n"
                "  idiv %rdi\n"
                "  mov %rdx,%rax\n",
};

static uint32_t toks_sz, toks_cap, text_sz, i;
static token_t *toks, *tok;
static char *text;

static var_t vars[MAX_VARS];
static fun_t funs[MAX_FUNS];
static token_t *strs[MAX_STRINGS];
static uint32_t nvars = 0, nfuns = 0, nstrs = 0;
static uint32_t nlabls = 0, cloop = 0;

/* XXX: small core library, semicolons, arrays, types, '&&', '||' */
void error(char *msg) {
    fprintf(stderr, "error: %s\n", msg);
    exit(1);
}

static void _append_token(token_t tok) {
    if (toks_sz >= toks_cap)
        toks = realloc(toks, sizeof(token_t)*(toks_cap += ALLOC_SZ));
    toks[toks_sz++] = tok;
}

#define PEEK(N) (text[i+(N)])
#define NEXT(N) (i += (N))
#define BOUND(N) (i+(N) < text_sz)

static void _skip_space(void) {
    while (BOUND(0) && isspace(PEEK(0))) NEXT(1);
    if (BOUND(0) && PEEK(0) == '#') {
        while (BOUND(0) && PEEK(0) != '\n') NEXT(1);
        _skip_space();
    }
}

static int _is_operator(char *buf, int sz) {
    int i;
    for (i = 0; i < LENGTH(OPERATORS); ++i) {
        if (strlen(OPERATORS[i]) != sz) continue;
        if (strncmp(OPERATORS[i], buf, sz) == 0) return i;
    }
    return -1;
}

static int _is_keyword(char *buf, int sz) {
    int i;
    for (i = 0; i < LENGTH(KEYWORDS); ++i) {
        if (strlen(KEYWORDS[i]) != sz) continue;
        if (strncmp(KEYWORDS[i], buf, sz) == 0) return i;
    }
    return -1;
}

static token_t _next_token(void) {
    token_t tok = {TK_EOF};
    _skip_space();
    if (!BOUND(0)) return tok;
    tok.ptr = text+i;
    if (isdigit(PEEK(0))) {
        tok.kind = TK_INT;
        while (BOUND(0) && isdigit(PEEK(0))) {
            ++tok.sz;
            NEXT(1);
        }
        return tok;
    } else if (PEEK(0) == '\'') {
        tok.kind = TK_CHR;
        ++tok.ptr;
        if (PEEK(1) == '\\') {
            ++tok.sz;
            NEXT(1);
        }
        ++tok.sz;
        NEXT(1);
        if (PEEK(1) != '\'') error("unclosed character");
        NEXT(2);
        return tok;
    } else if (PEEK(0) == '\"') {
        tok.kind = TK_STR;
        ++tok.ptr;
        NEXT(1);
        while (BOUND(0) && PEEK(0) != '\"') {
            if (PEEK(0) == '\\') {
                ++tok.sz;
                NEXT(1);
            }
            if (!BOUND(0)) error("unclosed string");
            ++tok.sz;
            NEXT(1);
        }
        NEXT(1);
        return tok;
    } else if (strchr(CHAROPS, PEEK(0)) != NULL) {
        int op1 = _is_operator(tok.ptr, 1);
        int op2 = _is_operator(tok.ptr, 2);
        if (op2 != -1) {
            tok.kind = op2+TK_LBRACK;
            tok.sz = 2;
        } else if (op1 != -1) {
            tok.kind = op1+TK_LBRACK;
            tok.sz = 1;
        } else error("unknown operator");
        NEXT(tok.sz);
        return tok;
    } else if (isalpha(PEEK(0)) || PEEK(0) == '_') {
        tok.kind = TK_ID;
        while (BOUND(0) && (isalnum(PEEK(0)) || PEEK(0) == '_')) {
            ++tok.sz;
            NEXT(1);
        }
        int kwrd = _is_keyword(tok.ptr, tok.sz);
        if (kwrd != -1) tok.kind = kwrd+TK_IF;
        if (tok.kind == TK_INLINE) {
            _skip_space();
            if (PEEK(0) != '{') error("missing '{'");
            NEXT(1);
            tok.ptr = text+i;
            tok.sz = 0;
            while (BOUND(0) && PEEK(0) != '}') {
                ++tok.sz;
                NEXT(1);
            }
            if (!BOUND(0)) error("missing '}'");
            NEXT(1);
            return tok;
        }
        return tok;
    }
    error("invalid character");
}

#undef PEEK
#undef NEXT
#undef BOUND

void parse_file(FILE *file) {
    fseek(file, 0, SEEK_END);
    text_sz = ftell(file);
    text = malloc(text_sz);
    fseek(file, 0, SEEK_SET);
    if (!fread(text, sizeof(char), text_sz, file)) error("could not read from file");
    i = 0;
    toks_sz = 0;
    toks = malloc(sizeof(token_t) * (toks_cap = ALLOC_SZ));
    token_t tok = {TK_EOF};
    do {
        tok = _next_token();
        _append_token(tok);
    } while (i < text_sz && tok.kind != TK_EOF);
}

#define NEXT(N) (tok += N)
#define PEEK(N) (tok + N)

static type_t _term(FILE *out);
static type_t _expr(FILE *out);
static void _stmt(FILE *out);
static type_t _binary(FILE *out, int prec);

static int _getfun(char *name, int sz) {
    int i;
    for (i = 0; i < nfuns; ++i) {
        if (sz != funs[i].sz) continue;
        if (!strncmp(funs[i].name, name, sz)) return i;
    }
    return -1;
}

static int _getvar(char *name, int sz) {
    int i;
    for (i = 0; i < nvars; ++i) {
        if (sz != vars[i].sz) continue;
        if (!strncmp(vars[i].name, name, sz)) return i;
    }
    return -1;
}

static int _getvarpos(int idx) {
    int i, sz = 0;
    for (i = 0; i <= idx; ++i) sz += vars[i].type.sz;
    return sz;
}

static int _newvar(char *name, int sz, type_t type) {
    vars[nvars].type = type;
    vars[nvars].sz = sz;
    vars[nvars].name = name;
    return _getvarpos(nvars++);
}

static type_t _gettype(char *ptr, int sz) {
    /* strings are just pointers */
    if (sz == 3 && !strncmp(ptr, "str", 3)) return _gettype("ptr", 3);
    int i;
    for (i = 0; i < types_sz; ++i) {
        if (sz != strlen(types[i].name)) continue;
        if (!strncmp(types[i].name, ptr, sz))
            return types[i];
    }
    return (type_t) { -1 };
}

static type_t _checktype(type_t fallback) {
    type_t type = fallback;
    if (PEEK(0)->kind == TK_COLON) {
        if (PEEK(1)->kind != TK_ID) error("expected type");
        type = _gettype(PEEK(1)->ptr, PEEK(1)->sz);
        if (type.sz == -1) error("unknown type");
        NEXT(2);
    }
    return type;
}

static void _newfun(fun_t fun) {
    funs[nfuns++] = fun;
}

static void _funhdr(char *name, int sz, FILE *out) {
    fprintf(out, "%.*s:\n  push %%rbp\n  mov %%rsp,%%rbp\n", sz, name);
    nvars = 0;
}

static void _funftr(FILE *out) {
    fprintf(out, ".L__%.*s.ret:\n  leave\n  ret\n",
        funs[nfuns-1].sz, funs[nfuns-1].name);
}

static type_t _ident(FILE *out, int lvalue) {
    token_t *name = PEEK(0);
    int idx, sz, cln = 0;
    type_t type = _gettype("int", 3), expr_type;
    NEXT(1);
    idx = _getvar(name->ptr, name->sz);
    /* variable type */
    if (PEEK(0)->kind == TK_COLON) {
        if (idx != -1) error("cannot declare variable twice");
        type = _checktype(_gettype("int", 3));
        cln = 1;
    }
    /* set variable */
    if (PEEK(0)->kind == TK_EQ) {
        if (lvalue == LVALUE_REF) error("unexpected '&'");
        if (idx == -1) {
            if (lvalue != LVALUE_NONE) error("undefined variable");
            idx = nvars;
            sz = _newvar(name->ptr, name->sz, type);
            fprintf(out, "  sub $%d,%%rsp\n", vars[idx].type.sz);
        } else {
            sz = _getvarpos(idx);
        }
        NEXT(1);
        expr_type = _expr(out);
        if (strcmp(expr_type.name, type.name) != 0) error("assignment of wrong type");
        if (lvalue == LVALUE_DEREF)
            fprintf(out, "  push %%rax\n  mov -%d(%%rbp),%%rax\n  pop (%%rax)\n", sz);
        else fprintf(out, "  mov %%rax,-%d(%%rbp)\n", sz);
        return type;
    }

    if (cln) error("unexpected ':'");
    /* call function */
    if (PEEK(0)->kind == TK_LPAREN) {
        int fidx = _getfun(name->ptr, name->sz);
        fun_t fun;
        if (fidx != -1) fun = funs[fidx];
        int arity = 0;
        type_t argtype;
        /* XXX: also check arity and argument types */
        type = (fidx != -1)? fun.ret : _gettype("int", 3);
        if (PEEK(1)->kind != TK_RPAREN) {
            do {
                if (arity >= MAX_ARGS) error("more than 6 arguments not supported yet");
                NEXT(1);
                argtype = _expr(out);
                if (fidx != -1) {
                    if (strcmp(argtype.name, fun.args[arity].name) != 0)
                        error("argument type mismatch");
                }
                fprintf(out, "  mov %%rax,%s\n", ARGREGS[arity++]);
            } while (PEEK(0)->kind == TK_COMMA);
        } else NEXT(1);
        if (PEEK(0)->kind != TK_RPAREN) error("missing ')'");
        NEXT(1);
        if (fidx != -1 && arity != fun.arity) error("mismatched arguments");
        fprintf(out, "  call %.*s\n", name->sz, name->ptr);
        return type;
    }
    /* get variable */
    if (idx == -1) error("undefined variable");
    type = (lvalue == LVALUE_REF || lvalue == LVALUE_DEREF)? _gettype("ptr", 3) : vars[idx].type;
    if (lvalue == LVALUE_REF)
        fprintf(out, "  lea -%d(%%rbp),%%rax\n", _getvarpos(idx));
    else if (lvalue == LVALUE_DEREF)
        fprintf(out, "  mov -%d(%%rbp),%%rax\n  mov (%%rax),%%rax\n", _getvarpos(idx));
    else fprintf(out, "  mov -%d(%%rbp),%%rax\n", _getvarpos(idx));
    return type;
}

static type_t _number(FILE *out) {
    int neg = PEEK(0)->kind == TK_SUB;
    if (neg) NEXT(1);
    fprintf(out, "  mov $%s%.*s,%%rax\n", neg?"-":"", PEEK(0)->sz, PEEK(0)->ptr);
    NEXT(1);
    return _gettype("int", 3);
}

static type_t _character(FILE *out) {
    token_t *chr = PEEK(0);
    int num = chr->ptr[0];
    /* escape character */
    if (chr->sz > 1) {
        if (chr->ptr[0] != '\\') error("invalid character");
        switch (chr->ptr[1]) {
        case '\\': num = '\\'; break;
        case '\'': num = '\''; break;
        case '\"': num = '\"'; break;
        case 'n': num = '\n'; break;
        case 'r': num = '\r'; break;
        case 't': num = '\t'; break;
        case '0': num = 0; break;
        default: error("invalid escape sequence");
        }
    }
    fprintf(out, "  mov $%d,%%rax\n", num);
    NEXT(1);
    return _gettype("char", 4);
}

static type_t _string(FILE *out) {
    strs[nstrs++] = PEEK(0);
    fprintf(out, "  mov $.L__string.%d,%%rax\n", nstrs-1);
    NEXT(1);
    return _gettype("str", 3);
}

static type_t _unary(FILE *out) {
    type_t type;
    switch (PEEK(0)->kind) {
    case TK_NOT:
        NEXT(1);
        type = _term(out);
        fprintf(out, "  not %%rax\n");
        break;
    case TK_BNOT:
        NEXT(1);
        type = _term(out);
        fprintf(out, "  cmp $0,%%rax\n  sete %%al\n  movzx  %%al,%%rax\n");
        break;
    case TK_SUB:
        if (PEEK(1)->kind == TK_INT) return _number(out);
        NEXT(1);
        type = _expr(out);
        fprintf(out, "  neg %%rax\n");
        break;
    case TK_AND:
        if (PEEK(1)->kind != TK_ID) error("expected identifier");
        NEXT(1);
        return _ident(out, LVALUE_REF);
    case TK_AT:
        NEXT(1);
        if (PEEK(0)->kind == TK_ID) return _ident(out, LVALUE_DEREF);
        type = _expr(out);
        fprintf(out, "  mov (%%rax),%%rax\n");
        break;
    default: error("unexpected operator");
    }
    return type;
}

static type_t _term(FILE *out) {
    switch (PEEK(0)->kind) {
    case TK_NOT: case TK_BNOT: case TK_SUB:
    case TK_AT: case TK_AND: return _unary(out);
    case TK_INT: return _number(out);
    case TK_CHR: return _character(out);
    case TK_STR: return _string(out);
    case TK_ID: return _ident(out, LVALUE_NONE);
    case TK_LPAREN: {
        NEXT(1);
        type_t type = _expr(out);
        if (PEEK(0)->kind != TK_RPAREN) error("missing ')'");
        NEXT(1);
        return type;
    }
    default: error("unexpected token");
    }
}

static int _precop(int kind, int prec) {
    int i;
    for (i = 0; i < LENGTH(PRECEDENCES[prec]); ++i)
        if (kind == PRECEDENCES[prec][i]) return 1;
    return 0;
}

static type_t _compare_expr(FILE *out, int op, int prec) {
    type_t type = _binary(out, prec+1);
    if (prec < PREC_COMPARE) error("&& and || are not implemented yet");
    fprintf(out, "  pop %%rdi\n  cmp %%rax,%%rdi\n  %s %%al\n  movzb %%al,%%rax\n", GENCMP[op]);
    return type;
}

static type_t _binary_expr(FILE *out, int op, int prec) {
    type_t type;
    if (prec == MAX_PRECEDENCE) type = _term(out);
    else type = _binary(out, prec+1);
    fprintf(out, "%s", GENOPS[op]);
    return type;
}

static type_t _binary(FILE *out, int prec) {
    type_t type;
    int op;
    if (prec == MAX_PRECEDENCE) type = _term(out);
    else type = _binary(out, prec+1);
    while (prec < MAX_PRECEDENCE && _precop(PEEK(0)->kind, prec)) {
        if ((op = PEEK(0)->kind) == TK_EOF) break;
        NEXT(1);
        fprintf(out, "  push %%rax\n");
        if (prec <= PREC_COMPARE) type = _compare_expr(out, op, prec);
        else type = _binary_expr(out, op, prec);
    }
    return type;
}

static type_t _expr(FILE *out) {
    return _binary(out, 0);
}

static void _body(FILE *out) {
    if (PEEK(0)->kind != TK_LBRACK) {
        if (PEEK(0)->kind >= TK_IF && PEEK(0)->kind <= TK_INLINE) _stmt(out);
        else _expr(out);
    } else {
        NEXT(1);
        while (PEEK(0)->kind != TK_RBRACK) {
            if (PEEK(0)->kind == TK_EOF) error("missing '}'");
            _stmt(out);
        }
        NEXT(1);
    }
}

static int _allocate_vars(void) {
    int in = 0, nvars = 0, idx = 0;
    token_t *cur = tok, *var;
    if (PEEK(0)->kind != TK_LBRACK) return 0;
    do {
        if (PEEK(0)->kind == TK_LBRACK) ++in;
        if (PEEK(0)->kind == TK_RBRACK) --in;
        if (PEEK(0)->kind == TK_ID) {
            var = PEEK(0);
            NEXT(1);
            if (PEEK(0)->kind == TK_EQ || PEEK(0)->kind == TK_COLON) {
                type_t type = _checktype(_gettype("int", 3));
                if (_getvar(PEEK(0)->ptr, PEEK(0)->sz) == -1)
                    idx = _newvar(PEEK(0)->ptr, PEEK(0)->sz, type);
            }
        }
        NEXT(1);
    } while (PEEK(0)->kind != TK_EOF && in > 0);
    tok = cur; /* restore position */
    return (idx == -1)? 0 : _getvarpos(idx);
}

static fun_t _mkfun(FILE *out, int local) {
    token_t *name = PEEK(1), *argname;
    type_t argtype;
    int i, idx = _getfun(name->ptr, name->sz);
    fun_t fun;
    if (name->kind != TK_ID) error("expected function name");
    if (idx != -1) error("cannot redefine function");
    NEXT(2); /* skip fun + name */
    fun.name = name->ptr;
    fun.sz = name->sz;
    fun.arity = 0;
    if (local) _funhdr(fun.name, fun.sz, out);
    if (PEEK(0)->kind != TK_LPAREN) error("missing '('");
    NEXT(1);
    if (PEEK(0)->kind == TK_ID) {
        argname = PEEK(0);
        NEXT(1);
        argtype = _checktype(_gettype("int", 3));
        if (local) _newvar(argname->ptr, argname->sz, argtype);
        fun.args[fun.arity++] = argtype;
        while (PEEK(0)->kind == TK_COMMA) {
            NEXT(1);
            if (PEEK(0)->kind != TK_ID) error("missing ')'");
            argname = PEEK(0);
            NEXT(1);
            argtype = _checktype(_gettype("int", 3));
            if (local) _newvar(argname->ptr, argname->sz, argtype);
            fun.args[fun.arity++] = argtype;
            if (fun.arity > MAX_ARGS)
                error("more than 6 arguments not supported yet");
        }
    }
    if (PEEK(0)->kind != TK_RPAREN) error("missing ')'");
    NEXT(1);
    fun.ret = _checktype(_gettype("int", 3));
    _newfun(fun);
    return fun;
}

static void _kwfun(FILE *out) {
    fun_t fun = _mkfun(out, 1);
    int nvars = _allocate_vars();
    if (fun.arity) {
        nvars += _getvarpos(fun.arity);
        fprintf(out, "  sub $%d,%%rsp\n", nvars);
        for (i = 0; i < fun.arity; ++i)
            fprintf(out, "  mov %s,-%d(%%rbp)\n", ARGREGS[i], _getvarpos(i));
    } else {
        fprintf(out, "  sub $%d,%%rsp\n", nvars);
    }
    _body(out);
    _funftr(out);
}

static void _kwreturn(FILE *out) {
    fun_t fun = funs[nfuns-1];
    NEXT(1);
    type_t type = _expr(out);
    if (strcmp(type.name, fun.ret.name) != 0) error("returning wrong type");
    fprintf(out, "  jmp .L__%.*s.ret\n", fun.sz, fun.name);
}

static void _kwextern(FILE *out) {
    do {
        NEXT(1);
        token_t *sym = PEEK(0);
        if (sym->kind == TK_FUN) {
            sym = PEEK(1);
            _mkfun(out, 0);
            NEXT(-1); /* womp womp */
        } else if (sym->kind != TK_ID) error("invalid external symbol");
        fprintf(out, ".extern %.*s\n", sym->sz, sym->ptr);
        NEXT(1);
    } while (PEEK(0)->kind == TK_COMMA);
}

static void _kwinline(FILE *out) {
    fprintf(out, "%.*s\n", PEEK(0)->sz, PEEK(0)->ptr);
    NEXT(1);
}

static void _kwbreak(FILE *out) {
    NEXT(1);
    if (cloop == 0) error("unexpected break");
    fprintf(out, "  jmp .L__while.end.%d\n", cloop-1);
}

static void _kwwhile(FILE *out) {
    int cur = nlabls++;
    cloop = nlabls;
    NEXT(1);
    fprintf(out, ".L__while.%d:\n", cur);
    _expr(out);
    fprintf(out, "  testq %%rax,%%rax\n  jz .L__while.end.%d\n", cur);
    _body(out);
    cloop = cur;
    fprintf(out, "  jmp .L__while.%d\n.L__while.end.%d:\n", cur, cur);
}

static void _kwif(FILE *out) {
    int cur = nlabls++;
    NEXT(1);
    _expr(out);
    fprintf(out, "  testq %%rax,%%rax\n"
                 "  jz .L__if.end.%d\n", cur);
    _body(out);
    fprintf(out, "  jmp .L__ifelse.end.%d\n"
                 ".L__if.end.%d:\n", cur, cur);
    if (PEEK(0)->kind == TK_ELSE) {
        NEXT(1);
        _body(out);
    }
    fprintf(out, ".L__ifelse.end.%d:\n", cur);
}

static void _stmt(FILE *out) {
    switch (PEEK(0)->kind) {
    case TK_FUN: _kwfun(out); break;
    case TK_RETURN: _kwreturn(out); break;
    case TK_EXTERN: _kwextern(out); break;
    case TK_INLINE: _kwinline(out); break;
    case TK_BREAK: _kwbreak(out); break;
    case TK_WHILE: _kwwhile(out); break;
    case TK_IF: _kwif(out); break;
    case TK_ID: _ident(out, LVALUE_NONE); break;
    case TK_AT: _unary(out); break;
    default: error("unexpected token");
    }
}

#undef PEEK
#undef NEXT

void gen_file(FILE *out) {
    tok = toks;
    fprintf(out, ".text\n.globl _start\n_start:\n  xor %%rax,%%rax\n  call main\n");
    fprintf(out, "  mov %%rax,%%rdi\n  mov $60,%%rax\n  syscall\n");
    do _stmt(out);
    while (tok->kind != TK_EOF);
    if (_getfun("main", 4) == -1) error("missing 'main' function");
    int i;
    fprintf(out, ".data\n");
    for (i = 0; i < nstrs; ++i) {
        token_t *tok = strs[i];
        fprintf(out, ".L__string.%d: .asciz \"%.*s\"\n.align 8\n",
            i, tok->sz, tok->ptr);
    }
}

static FILE *_open(const char *path, const char *mode) {
    FILE *fp = fopen(path, mode);
    if (!fp) {
        fprintf(stderr, "error: could not open file '%s'\n", path);
        exit(1);
    }
    return fp;
}

static void _runasm(const char *path, const char *out) {
    char cmd[ALLOC_SZ] = {0};
    sprintf(cmd, "as %s nlib.s -o %s.o && ld %s.o -o %s && rm %s %s.o",
        path, path, path, out, path, path);
    if (system(cmd)) exit(1);
}

void usage(const char *prog) {
    printf("usage: %s [-h|-s|-o output] <input>\n", prog);
    exit(0);
}

int main(int argc, const char **argv) {
    FILE *input_file = NULL, *output_file = NULL;
    const char *input_path = NULL, *output_path = NULL;
    char output_asm[ALLOC_SZ] = {0};
    int i, output_stdout = 0;

    for (i = 1; i < argc; ++i) {
        if (!strcmp(argv[i], "-h")) usage(argv[0]);
        else if (!strcmp(argv[i], "-s"))
            output_stdout = 1;
        else if (!strcmp(argv[i], "-o"))
            output_path = argv[++i];
        else input_path = argv[i];
    }
    if (!input_path) usage(argv[0]);
    sprintf(output_asm, "%s.s", input_path? input_path : "a");
    input_file = _open(input_path, "r");
    output_file = output_stdout? stdout : _open(output_asm, "w+");
    parse_file(input_file);
    gen_file(output_file);
    fclose(input_file);
    fclose(output_file);
    free(toks);
    free(text);
    if (!output_stdout) _runasm(output_asm, (output_path)? output_path : "a.out");
    return 0;
}

