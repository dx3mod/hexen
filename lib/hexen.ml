(** Note.

    This code primarily uses "unsafe" functions to increase performance by
    avoiding excessive overhead costs. It is dangerous! *)

let lowercase_hex_map = "0123456789abcdef"
let uppercase_hex_map = "0123456789ABCDEF"

(* Utils *)

let[@inline] required_length_for_encode len = len * 2
let[@inline] required_length_for_decode len = len / 2

let[@inline] check_range src_len off len =
  if off < 0 || len < 0 || len > src_len - off then
    invalid_arg "hexen: invalid buffer ranges"

let[@inline] check_leftover len =
  if len mod 2 <> 0 then invalid_arg "hexen: leftover byte in hex string"

(* Encoding *)

let encode_into ?(uppercase = false) src src_off dst dst_off len =
  check_range (String.length src) src_off len;
  check_range (Bytes.length dst) dst_off (required_length_for_encode len);

  let hex_map = if uppercase then uppercase_hex_map else lowercase_hex_map in

  for i = 0 to len - 1 do
    let char_code = int_of_char @@ String.unsafe_get src (i + src_off) in
    let i' = (i * 2) + dst_off in

    let hex_hi = String.unsafe_get hex_map (char_code lsr 4)
    and hex_lo = String.unsafe_get hex_map (char_code land 0x0F) in

    Bytes.unsafe_set dst i' hex_hi;
    Bytes.unsafe_set dst (succ i') hex_lo
  done

let encode_substring_to_buffer ?(uppercase = false) buffer src src_off len =
  check_range (String.length src) src_off len;

  let hex_map = if uppercase then uppercase_hex_map else lowercase_hex_map in

  for i = 0 to len - 1 do
    let char_code = int_of_char @@ String.unsafe_get src (i + src_off) in

    let hex_hi = String.unsafe_get hex_map (char_code lsr 4)
    and hex_lo = String.unsafe_get hex_map (char_code land 0x0F) in

    Buffer.add_char buffer hex_hi;
    Buffer.add_char buffer hex_lo
  done

let encode_to_buffer ?uppercase buffer source =
  encode_substring_to_buffer ?uppercase buffer source 0 (String.length source)

let encode_substring ?uppercase source off len =
  match len with
  | 0 -> ""
  | _ ->
      let bytes = Bytes.create (required_length_for_encode len) in
      encode_into ?uppercase source off bytes 0 len;
      Bytes.unsafe_to_string bytes

let encode ?uppercase source =
  encode_substring ?uppercase source 0 (String.length source)

(* Decoding *)

let int_of_hex_digit ch =
  let char_code = int_of_char ch in
  match ch with
  | '0' .. '9' -> char_code - 0x30
  | 'A' .. 'F' -> char_code - 0x37 (* 0x41 - 10 *)
  | 'a' .. 'f' -> char_code - 0x57 (* 0x61 - 10 *)
  | _ -> invalid_arg "hexen: bad character in hex string"

let hex_lookup_table =
  "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\xff\xff\xff\xff\xff\xff\xff\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"

let decode_into src src_off dst dst_off len =
  check_leftover len;
  check_range (String.length src) src_off len;
  check_range (Bytes.length dst) dst_off (required_length_for_decode len);

  let error_acc = ref 0 in

  for i = 0 to required_length_for_decode len - 1 do
    let i_src = (i * 2) + src_off in

    let hex_hi = int_of_char @@ String.unsafe_get src i_src in
    let hex_lo = int_of_char @@ String.unsafe_get src (i_src + 1) in

    let hi = int_of_char @@ String.unsafe_get hex_lookup_table hex_hi in
    let lo = int_of_char @@ String.unsafe_get hex_lookup_table hex_lo in

    error_acc := !error_acc lor hi lor lo;

    Bytes.unsafe_set dst (i + dst_off) (Obj.magic ((hi lsl 4) lor lo))
  done;

  if !error_acc = 255 then invalid_arg "hexen: bad character in hex string"

let decode_substring_to_buffer buffer src src_off len =
  check_leftover len;
  check_range (String.length src) src_off len;

  for i = 0 to required_length_for_decode len - 1 do
    let i_src = (i * 2) + src_off in

    let hex_hi = String.unsafe_get src i_src
    and hex_lo = String.unsafe_get src (succ i_src) in

    let hi = int_of_hex_digit hex_hi lsl 4 and lo = int_of_hex_digit hex_lo in

    Buffer.add_uint8 buffer (hi lor lo)
  done

let decode_to_buffer buffer hex =
  decode_substring_to_buffer buffer hex 0 (String.length hex)

let decode_substring hex off len =
  match len with
  | 0 -> ""
  | _ ->
      let bytes = Bytes.create (required_length_for_decode len) in
      decode_into hex off bytes 0 len;
      Bytes.unsafe_to_string bytes

let decode hex = decode_substring hex 0 (String.length hex)
