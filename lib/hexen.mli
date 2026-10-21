(** Hexadecimal encoding and decoding library.

    The library provides fast and low-allocation utilities for encoding raw
    binary strings into hexadecimal representations and decoding them. *)

(** {2 Encode} *)

val encode : ?uppercase:bool -> string -> string
(** [encode ?uppercase source] encodes the [source] string to new hexadecimal
    string.

    @raise Invalid_argument *)

val encode_substring : ?uppercase:bool -> string -> int -> int -> string
(** [encode_substring ?uppercase source off len] encodes the [source] substring
    to new hexadecimal string.

    @raise Invalid_argument *)

val encode_to_buffer : ?uppercase:bool -> Buffer.t -> string -> unit
(** [encode_to_buffer ?uppercase buffer source] encodes the [source] string to
    the [buffer].

    @raise Invalid_argument *)

val encode_substring_to_buffer :
  ?uppercase:bool -> Buffer.t -> string -> int -> int -> unit
(** [encode_substring_to_buffer ?uppercase buffer source off len] encodes the
    [source] substring to the [buffer].

    @raise Invalid_argument *)

val encode_into :
  ?uppercase:bool -> string -> int -> bytes -> int -> int -> unit
(** [encode_into ?uppercase src src_off dst dst_off len] encodes the [src]
    substring to the [dst] bytes range.

    @raise Invalid_argument *)

(** {2 Decode} *)

val decode : string -> string
(** [decode hexadecimal] decodes the [hexadecimal] string to new binary string.

    @raise Invalid_argument *)

val decode_substring : string -> int -> int -> string
(** [decode_substring hexadecimal off len] decodes the [hexadecimal] substring
    to new binary string.

    @raise Invalid_argument *)

val decode_to_buffer : Buffer.t -> string -> unit
(** [decode_to_buffer buffer hexadecimal] decodes the [hexadecimal] string to
    the [buffer].

    @raise Invalid_argument *)

val decode_substring_to_buffer : Buffer.t -> string -> int -> int -> unit
(** [decode_substring_to_buffer buffer hexadecimal off len] decodes the
    [hexadecimal] substring to the [buffer].

    @raise Invalid_argument *)

val decode_into : string -> int -> bytes -> int -> int -> unit
(** [decode_into sec src_off dst dst_off len] decodes the [src] hexadecimal
    substring to the [dst] bytes range.

    @raise Invalid_argument *)
