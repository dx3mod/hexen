(** Hexadecimal encoding and decoding library.

    This library provides fast, low-allocation utilities for encoding raw binary
    strings into hexadecimal representations and decoding them back.

    Performance note: this implementation relies on unsafe functions internally
    to maximize performance by avoiding bounds-checking overhead. Incorrect
    usage may lead to undefined behavior. *)

(** {2 Encode} *)

val encode : ?uppercase:bool -> string -> string
(** [encode ?uppercase source] encodes the [source] string into a new
    hexadecimal string.

    If [uppercase] is [true], the letters [A-F] in the output are uppercase;
    otherwise they are lowercase (the default). *)

val encode_substring : ?uppercase:bool -> string -> int -> int -> string
(** [encode_substring ?uppercase source off len] encodes the substring of
    [source] starting at offset [off] and of length [len] into a new hexadecimal
    string.

    @raise Invalid_argument
      if [off] or [len] is negative, or if [off + len] exceeds the length of
      [source]. *)

val encode_to_buffer : ?uppercase:bool -> Buffer.t -> string -> unit
(** [encode_to_buffer ?uppercase buffer source] encodes the [source] string to
    the [buffer].

    @raise Invalid_argument *)

val encode_substring_to_buffer :
  ?uppercase:bool -> Buffer.t -> string -> int -> int -> unit
(** [encode_substring_to_buffer ?uppercase buffer source off len] encodes the
    substring of [source] starting at offset [off] and of length [len], into the
    [buffer].

    @raise Invalid_argument
      if [off] or [len] is negative, or if [off + len] exceeds the length of
      [source]. *)

val encode_into :
  ?uppercase:bool -> string -> int -> bytes -> int -> int -> unit
(** [encode_into ?uppercase src src_off dst dst_off len] encodes the substring
    of [src] starting at offset [src_off] and of length [len], and writes the
    resulting hexadecimal bytes into [dst] starting at offset [dst_off].

    @raise Invalid_argument
      if [src_off] or [len] is negative, if [src_off + len] exceeds the length
      of [src], if [dst_off] is negative, or if [dst_off + (len * 2)] exceeds
      the length of [dst]. *)

(** {2 Decode} *)

val decode : string -> string
(** [decode hex] decodes the hexadecimal [hex] string into a new binary string.
    Both uppercase and lowercase letters [A-F] / [a-f] are accepted.

    @raise Invalid_argument
      if the length of [hex] is odd, or if it contains a character that is not a
      valid hexadecimal digit. *)

val decode_substring : string -> int -> int -> string
(** [decode_substring hex off len] decodes the substring of [hex] starting at
    offset [off] and of length [len] into a new binary string.

    @raise Invalid_argument
      if [len] is odd, if [off] or [len] is negative, if [off + len] exceeds the
      length of [hex], or if the substring contains a character that is not a
      valid hexadecimal digit. *)

val decode_to_buffer : Buffer.t -> string -> unit
(** [decode_to_buffer buffer hex] decodes the hexadecimal [hex] string to the
    [buffer].

    @raise Invalid_argument
      if the length of [hex] is odd, or if it contains a character that is not a
      valid hexadecimal digit. *)

val decode_substring_to_buffer : Buffer.t -> string -> int -> int -> unit
(** [decode_substring_to_buffer buffer hex off len] decodes the substring of
    [hex] starting at offset [off] and of length [len], to the [buffer].

    @raise Invalid_argument
      if [len] is odd, if [off] or [len] is negative, if [off + len] exceeds the
      length of [hex], or if the substring contains a character that is not a
      valid hexadecimal digit. *)

val decode_into : string -> int -> bytes -> int -> int -> unit
(** [decode_into src src_off dst dst_off len] decodes the substring of [src]
    starting at offset [src_off] and of length [len], and writes the resulting
    binary bytes into [dst] starting at offset [dst_off].

    @raise Invalid_argument
      if [len] is odd, if [src_off] or [len] is negative, if [src_off + len]
      exceeds the length of [src], if [dst_off] is negative, if
      [dst_off + (len / 2)] exceeds the length of [dst], or if the substring
      contains a character that is not a valid hexadecimal digit. *)
