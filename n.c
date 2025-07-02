#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define LENGTH(X) (sizeof(X)/sizeof((X)[0]))
#define ALLOC_SZ 1024

enum {
    TK_EOF, TK_ID, TK_INT, TK_STR, TK_CHR,
    TK_IF, TK_ELSE, TK_WHILE, TK_BREAK,
    TK_FUN, TK_RETURN, TK_EXTERN,
    TK_LBRACK, TK_RBRACK, TK_LPAREN, TK_RPAREN,
    TK_COMMA, TK_ADD, TK_SUB, TK_MUL, TK_DIV, TK_MOD,
    TK_BAND, TK_BOR, TK_BNOT, TK_EQ, TK_EQEQ, TK_NEQ,
    TK_LT, TK_GT, TK_LEQ, TK_GEQ, TK_SHL, TK_SHR,
    TK_AND, TK_OR, TK_NOT, TK_XOR, MAX_TOKENS
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

#define MAX_ARGS 6
static const char *ARGREGS[MAX_ARGS] = {
    "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9",
};

static const char *CHAROPS = "{}(),+-*/%&|!=<>~^";
static const char *OPERATORS[] = {
    "{", "}", "(", ")", ",", "+", "-", "*", "/", "%",
    "&&", "||", "!", "=", "==", "!=", "<", ">", "<=",
    ">=", "<<", ">>", "&", "|", "~", "^",
};

static const char *KEYWORDS[] = {
    "if", "else", "while", "break",
    "fun", "return", "extern",
};

typedef struct {
    uint32_t kind, sz;
    char *ptr;
} token_t;

typedef struct {
    uint32_t type_sz, sz;
    char *name;
} var_t;

typedef struct {
    uint32_t arity, sz;
    char *name;
} fun_t;

typedef struct {
    uint32_t toks_sz, toks_cap, text_sz, i;
    token_t *toks, *tok;
    char *text;
} gen_t;

static const char *_gencmp[MAX_TOKENS] = {
    [TK_EQEQ] = "sete",
    [TK_NEQ]  = "setne",
    [TK_LT]   = "setl",
    [TK_LEQ]  = "setle",
    [TK_GT]   = "setg",
    [TK_GEQ]  = "setge",
};

static const char *_genops[MAX_TOKENS] = {
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

#define MAX_VARS 256
static var_t vars[MAX_VARS];
static uint32_t nvars = 0;

#define MAX_FUNS 1024
static fun_t funs[MAX_FUNS];
static uint32_t nfuns = 0;

#define MAX_STRINGS 1024
static token_t *strs[MAX_STRINGS];
static uint32_t nstrs = 0;
static uint32_t nconds = 0, nloops = 0, curloop = 0;

/* XXX: small core library, '&&', '||' */

void error(char *msg) {
    fprintf(stderr, "error: %s\n", msg);
    exit(1);
}

static void _append_token(gen_t *gen, token_t tok) {
    if (gen->toks_sz >= gen->toks_cap)
        gen->toks = realloc(gen->toks, sizeof(token_t)*(gen->toks_cap += ALLOC_SZ));
    gen->toks[gen->toks_sz++] = tok;
}

#define PEEK(N) (gen->text[gen->i+(N)])
#define NEXT(N) (gen->i += (N))
#define BOUND(N) (gen->i+(N) < gen->text_sz)

static void _skip_space(gen_t *gen) {
    while (BOUND(0) && isspace(PEEK(0))) NEXT(1);
    if (BOUND(0) && PEEK(0) == '#') {
        while (BOUND(0) && PEEK(0) != '\n') NEXT(1);
        _skip_space(gen);
    }
}

static int isoperator(char *buf, int sz) {
    int i;
    for (i = 0; i < LENGTH(OPERATORS); ++i) {
        if (strlen(OPERATORS[i]) != sz) continue;
        if (strncmp(OPERATORS[i], buf, sz) == 0) return i;
    }
    return -1;
}

static int iskeyword(char *buf, int sz) {
    int i;
    for (i = 0; i < LENGTH(KEYWORDS); ++i) {
        if (strlen(KEYWORDS[i]) != sz) continue;
        if (strncmp(KEYWORDS[i], buf, sz) == 0) return i;
    }
    return -1;
}

static token_t _next_token(gen_t *gen) {
    token_t tok = {TK_EOF};
    _skip_space(gen);
    if (!BOUND(0)) return tok;
    tok.ptr = gen->text+gen->i;
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
        int op1 = isoperator(tok.ptr, 1);
        int op2 = isoperator(tok.ptr, 2);
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
        int kwrd = iskeyword(tok.ptr, tok.sz);
        if (kwrd != -1) tok.kind = kwrd+TK_IF;
        return tok;
    }
    error("invalid character");
}

#undef PEEK
#undef NEXT
#undef BOUND

void parse_file(gen_t *gen, FILE *file) {
    fseek(file, 0, SEEK_END);
    gen->text_sz = ftell(file);
    gen->text = malloc(gen->text_sz);
    fseek(file, 0, SEEK_SET);
    if (!fread(gen->text, sizeof(char), gen->text_sz, file))
        error("could not read from file");
    gen->i = 0;
    gen->toks = malloc(sizeof(token_t) * (gen->toks_cap = ALLOC_SZ));
    gen->toks_sz = 0;
    token_t tok = {TK_EOF};
    do {
        tok = _next_token(gen);
        _append_token(gen, tok);
    } while (gen->i < gen->text_sz && tok.kind != TK_EOF);
}

void free_gen(gen_t *gen) {
    free(gen->toks);
    free(gen->text);
}

#define NEXT(N) (gen->tok += N)
#define PEEK(N) (gen->tok + N)

static void _term(gen_t *gen, FILE *out);
static void _expr(gen_t *gen, FILE *out);
static void _stmt(gen_t *gen, FILE *out);
static void _binary(gen_t *gen, FILE *out, int prec);

static int _getfun(char *name, int sz) {
    int i;
    for (i = 0; i < nfuns; ++i) {
        if (sz != funs[i].sz) continue;
        if (!strncmp(funs[i].name, name, sz))
            return i;
    }
    return -1;
}

static int _getvar(char *name, int sz) {
    int i;
    for (i = 0; i < nvars; ++i) {
        if (sz != vars[i].sz) continue;
        if (!strncmp(vars[i].name, name, sz))
            return i;
    }
    return -1;
}

static int _getvarpos(int idx) {
    int i, sz = 0;
    for (i = 0; i <= idx; ++i) sz += vars[i].type_sz;
    return sz;
}

static int _newvar(char *name, int sz, int typ) {
    vars[nvars].type_sz = typ;
    vars[nvars].sz = sz;
    vars[nvars].name = name;
    return _getvarpos(nvars++);
}

static void _funhdr(char *name, int sz, FILE *out) {
    fprintf(out, "%.*s:\n  push %%rbp\n  mov %%rsp,%%rbp\n", sz, name);
    funs[nfuns].sz = sz;
    funs[nfuns].name = name;
    ++nfuns;
    nvars = 0;
}

static void _funftr(FILE *out) {
    fprintf(out, "  leave\n  ret\n");
}

static void _ident(gen_t *gen, FILE *out) {
    token_t *name = PEEK(0);
    int idx, sz;
    NEXT(1);
    idx = _getvar(name->ptr, name->sz);
    switch (PEEK(0)->kind) {
    /* call function */
    case TK_LPAREN: {
        int arity = 0;
        if (PEEK(0)->kind != TK_RPAREN) {
            do {
                if (arity >= MAX_ARGS) {
                    fprintf(stderr, "error: more than %d arguments is not supported yet\n", MAX_ARGS);
                    exit(1);
                }
                NEXT(1);
                _expr(gen, out);
                fprintf(out, "  mov %%rax,%s\n", ARGREGS[arity++]);
            } while (PEEK(0)->kind == TK_COMMA);
        }
        if (PEEK(0)->kind != TK_RPAREN) error("missing ')'");
        NEXT(1);
        fprintf(out, "  call %.*s\n", name->sz, name->ptr);
    } break;
    /* set variable */
    case TK_EQ: {
        /* XXX: proper types/type sizes */
        if (idx == -1) {
            idx = nvars;
            sz = _newvar(name->ptr, name->sz, 8);
            fprintf(out, "  sub $%d,%%rsp\n", vars[idx].type_sz);
        } else {
            sz = _getvarpos(idx);
        }
        NEXT(1);
        _expr(gen, out);
        fprintf(out, "  mov %%rax,-%d(%%rbp)\n", sz);
    } break;
    /* get variable */
    default:
        if (idx == -1) {
            fprintf(stderr, "error: undefined variable '%.*s'\n",
                name->sz, name->ptr);
            exit(1);
        }
        fprintf(out, "  mov -%d(%%rbp),%%rax\n", _getvarpos(idx));
    }
}

static void _number(gen_t *gen, FILE *out) {
    int neg = PEEK(0)->kind == TK_SUB;
    if (neg) NEXT(1);
    fprintf(out, "  mov $%s%.*s,%%rax\n", neg?"-":"", PEEK(0)->sz, PEEK(0)->ptr);
    NEXT(1);
}

static void _character(gen_t *gen, FILE *out) {
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
}

static void _string(gen_t *gen, FILE *out) {
    strs[nstrs++] = PEEK(0);
    fprintf(out, "  mov $.string.%d,%%rax\n", nstrs-1);
    NEXT(1);
}

static void _unary(gen_t *gen, FILE *out) {
    switch (PEEK(0)->kind) {
    case TK_NOT:
        NEXT(1);
        _term(gen, out);
        fprintf(out, "  not %%rax\n");
        break;
    case TK_BNOT:
        NEXT(1);
        _term(gen, out);
        fprintf(out, "  cmp $0,%%rax\n  sete %%al\n  movzx  %%al,%%rax\n");
        break;
    case TK_SUB:
        if (PEEK(1)->kind == TK_INT)
            return _number(gen, out);
        NEXT(1);
        _expr(gen, out);
        fprintf(out, "  neg %%rax\n");
        break;
    default:
        fprintf(stderr, "error: unexpected '%.*s'\n",
            PEEK(0)->sz, PEEK(0)->ptr);
        exit(1);
    }
}

static void _term(gen_t *gen, FILE *out) {
    switch (PEEK(0)->kind) {
    case TK_NOT: case TK_BNOT: case TK_SUB:
        return _unary(gen, out);
    case TK_INT: return _number(gen, out);
    case TK_CHR: return _character(gen, out);
    case TK_STR: return _string(gen, out);
    case TK_ID: return _ident(gen, out);
    case TK_LPAREN:
        NEXT(1);
        _expr(gen, out);
        if (PEEK(0)->kind != TK_RPAREN) error("missing ')'");
        NEXT(1);
        return;
    default:
        fprintf(stderr, "error: unexpected '%.*s'\n",
            PEEK(0)->sz, PEEK(0)->ptr);
        exit(1);
    }
}

static int _precop(int kind, int prec) {
    int i;
    for (i = 0; i < LENGTH(PRECEDENCES[prec]); ++i)
        if (kind == PRECEDENCES[prec][i]) return 1;
    return 0;
}

static void _compare_expr(gen_t *gen, FILE *out, int op, int prec) {
    _binary(gen, out, prec+1);
    if (prec < PREC_COMPARE) error("&& and || are not implemented yet");
    fprintf(out, 
        "  pop %%rdi\n"
        "  cmp %%rax,%%rdi\n"
        "  %s %%al\n"
        "  movzb %%al,%%rax\n",
        _gencmp[op]);
}

static void _binary_expr(gen_t *gen, FILE *out, int op, int prec) {
    if (prec == MAX_PRECEDENCE) _term(gen, out);
    else _binary(gen, out, prec+1);
    fprintf(out, "%s", _genops[op]);
}

static void _binary(gen_t *gen, FILE *out, int prec) {
    int op;
    if (prec == MAX_PRECEDENCE) _term(gen, out);
    else _binary(gen, out, prec+1);
    while (prec < MAX_PRECEDENCE && _precop(PEEK(0)->kind, prec)) {
        if ((op = PEEK(0)->kind) == TK_EOF) break;
        NEXT(1);
        fprintf(out, "  push %%rax\n");
        if (prec <= PREC_COMPARE) _compare_expr(gen, out, op, prec);
        else _binary_expr(gen, out, op, prec);
    }
}

static void _expr(gen_t *gen, FILE *out) {
    _binary(gen, out, 0);
}

static void _body(gen_t *gen, FILE *out) {
    if (PEEK(0)->kind != TK_LBRACK) {
        if (PEEK(0)->kind >= TK_IF && PEEK(0)->kind <= TK_EXTERN) _stmt(gen, out);
        else _expr(gen, out);
    } else {
        NEXT(1);
        while (PEEK(0)->kind != TK_RBRACK) {
            if (PEEK(0)->kind == TK_EOF) error("missing '}'");
            _stmt(gen, out);
        }
        NEXT(1);
    }
}

static void _kwfun(gen_t *gen, FILE *out) {
    token_t *name = PEEK(1);
    if (name->kind != TK_ID) error("expected function name");
    NEXT(2); /* skip fun + name */
    int i, arg, idx = _getfun(name->ptr, name->sz);
    _funhdr(name->ptr, name->sz, out);
    /* XXX: preallocate all local variables in a single instruction */
    if (PEEK(0)->kind != TK_LPAREN) error("missing '('");
    NEXT(1);
    funs[nfuns].arity = 0;
    if (PEEK(0)->kind == TK_ID) {
        /* XXX: types/type sizes */
        arg = _newvar(PEEK(0)->ptr, PEEK(0)->sz, 8);
        NEXT(1);
        ++funs[nfuns].arity;
        while (PEEK(0)->kind == TK_COMMA) {
            NEXT(1);
            if (PEEK(0)->kind != TK_ID) error("missing ')'");
            arg = _newvar(PEEK(0)->ptr, PEEK(0)->sz, 8);
            NEXT(1);
            if (++funs[nfuns].arity > MAX_ARGS) {
                fprintf(stderr, "error: more than %d arguments are not supported yet\n", MAX_ARGS);
                exit(1);
            }
        }
        fprintf(out, "  sub $%d,%%rsp\n", arg);
        for (i = 0; i < nvars; ++i)
            fprintf(out, "  mov %s,-%d(%%rbp)\n", ARGREGS[i], _getvarpos(i));
    }
    if (PEEK(0)->kind != TK_RPAREN) error("missing ')'");
    NEXT(1);
    if (idx != -1) {
        fprintf(stderr, "error: redefinition of function '%.*s'\n",
            name->sz, name->ptr);
        exit(1);
    }

    _body(gen, out);
    _funftr(out);
}

static void _kwreturn(gen_t *gen, FILE *out) {
    NEXT(1);
    _expr(gen, out);
    _funftr(out);
}

static void _kwextern(gen_t *gen, FILE *out) {
    do {
        NEXT(1);
        if (PEEK(0)->kind != TK_ID) error("invalid external symbol");
        fprintf(out, ".extern %.*s\n", PEEK(0)->sz, PEEK(0)->ptr);
        NEXT(1);
    } while (PEEK(0)->kind == TK_COMMA);
}

static void _kwbreak(gen_t *gen, FILE *out) {
    NEXT(1);
    if (curloop == 0) error("unexpected break");
    fprintf(out, "  jmp .while_end.%d\n", curloop-1);
}

static void _kwwhile(gen_t *gen, FILE *out) {
    int cloop = nloops++;
    curloop = nloops;
    NEXT(1);
    fprintf(out, ".while.%d:\n", cloop);
    _expr(gen, out);
    fprintf(out, "  testq %%rax,%%rax\n"
                 "  jz .while_end.%d\n", cloop);
    _body(gen, out);
    curloop = cloop;
    fprintf(out, "  jmp .while.%d\n"
                 ".while_end.%d:\n", cloop, cloop);
}

static void _kwif(gen_t *gen, FILE *out) {
    int cif = nconds++;
    NEXT(1);
    _expr(gen, out);
    fprintf(out, "  testq %%rax,%%rax\n"
                 "  jz .if_end.%d\n", cif);
    _body(gen, out);
    fprintf(out, "  jmp .ifelse_end.%d\n"
                 ".if_end.%d:\n", cif, cif);
    if (PEEK(0)->kind == TK_ELSE) {
        NEXT(1);
        _body(gen, out);
    }
    fprintf(out, ".ifelse_end.%d:\n", cif);
}

static void _stmt(gen_t *gen, FILE *out) {
    switch (PEEK(0)->kind) {
    case TK_FUN: return _kwfun(gen, out);
    case TK_RETURN: return _kwreturn(gen, out);
    case TK_EXTERN: return _kwextern(gen, out);
    case TK_BREAK: return _kwbreak(gen, out);
    case TK_WHILE: return _kwwhile(gen, out);
    case TK_IF: return _kwif(gen, out);
    case TK_ID: return _ident(gen, out);
    default:
        fprintf(stderr, "error: unexpected '%.*s'\n",
            PEEK(0)->sz, PEEK(0)->ptr);
        exit(1);
    }
}

#undef PEEK
#undef NEXT

void gen_file(gen_t *gen, FILE *out) {
    gen->tok = gen->toks;
    fprintf(out, ".text\n.globl _start\n_start:\n  xor %%rax,%%rax\n  call main\n");
    fprintf(out, "  mov %%rax,%%rdi\n  mov $60,%%rax\n  syscall\n");
    do {
        _stmt(gen, out);
        /*_decl(gen, file);*/
    } while (gen->tok->kind != TK_EOF);
    if (_getfun("main", 4) == -1) error("missing 'main' function");
    int i;
    fprintf(out, ".data\n");
    for (i = 0; i < nstrs; ++i) {
        token_t *tok = strs[i];
        fprintf(out, ".string.%d: .asciz \"%.*s\"\n.align 8\n",
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
    char cmd[1024] = {0};
    sprintf(cmd, "as %s nlib.s -o %s.o && ld %s.o -o %s && rm %s %s.o",
        path, path, path, out, path, path);
    if (system(cmd));
}

void usage(const char *prog) {
    printf("usage: %s <input> [-h|-s|-c|-o output]\n", prog);
    exit(0);
}

/* XXX: -c should assemble but not link, -s should generate assembly but not assemble */
int main(int argc, const char **argv) {
    FILE *input_file = NULL, *output_file = NULL;
    const char *input_path = NULL, *output_path = NULL;
    char output_asm[1024] = {0};
    int i, assemble = 1, output_stdout = 0;
    if (argc < 2) usage(argv[0]);

    for (i = 1; i < argc; ++i) {
        if (!strcmp(argv[i], "-h")) usage(argv[0]);
        else if (!strcmp(argv[i], "-c"))
            assemble = 0;
        else if (!strcmp(argv[i], "-s"))
            output_stdout = 1;
        else if (!strcmp(argv[i], "-o"))
            output_path = argv[++i];
        else
            input_path = argv[i];
    }
    gen_t gen = {0};

    sprintf(output_asm, "%s.s", input_path? input_path : "a");
    input_file = _open(input_path, "r");
    output_file = output_stdout? stdout : _open(output_asm, "w+");

    parse_file(&gen, input_file);
    gen_file(&gen, output_file);

    fclose(input_file);
    fclose(output_file);
    if (assemble && !output_stdout) _runasm(output_asm, (output_path)? output_path : "a.out");
    free_gen(&gen);
    return 0;
}

