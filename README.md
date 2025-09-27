# nlang
Toy compiler for a tiny language that i made for fun. It is not a serious project.  
I didn't feel like thinking up a name so i picked a random letter (like all good languages do).  
(Only targets x86_64 linux btw).

## Installation
Compile with:
```sh
make install
```

## Example
Hello world:
```n
fun main() {
    extern putstr;
    putstr("Hello, World!\n");
}
```

Compile with:
```sh
./n <file>
```
More examples can be found [here](/examples).
