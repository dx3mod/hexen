# Hexen 

Yet another library for encoding and decoding hexadecimal byte sequences.
 

## Quick start

You can install `hexen` using the [OPAM] package manager or another method. If you are using [Dune], add `hexen` to your dependencies.

### In use

Hexen provides high-level functions like `encode` and `decode` for quick encoding and decoding of strings or substrings.

```ocaml
Hexen.encode "Hello World";;
(* - : string = "48656c6c6f20576f726c64" *)

Hexen.decode "48656c6c6f20576f726c64";;
(* - : string = "Hello World" *)
```

```ocaml
Hexen.encode_substring "Hello World" 0 5;;
(* - : string = "48656c6c6f" *)

Hexen.decode_substring "48656c6c6f20576f726c64" 0 10;;
(* - : string = "Hello" *)
```

Also, Hexen provides functions to encode and decode strings or substrings directly into existing buffers or bytes.

```ocaml
let buffer = Buffer.create 16 in

Hexen.encode_to_buffer buffer "Hello";
Hexen.encode_substring_to_buffer buffer "Hello World" 5 6;

let bytes = Bytes.create 12 in 
Hexen.decode_into (Buffer.contents buffer) 0 bytes 0 22;
Bytes.set bytes 11 '!';

bytes
(* - : bytes = Bytes.of_string "Hello World!" *)
```

## Credits

This library was heavily inspired by the [Ohex](https://github.com/robur-coop/ohex) library and its codebase.


## License

The project is licensed under [the MIT License](./LICENSE), which allows for all permissions.
Just use it and enjoy yourself without fear. We are always open to pull requests!

[OPAM]: https://opam.ocaml.org/
[Dune]: https://dune.build