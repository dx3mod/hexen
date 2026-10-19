let lowercase_hex_map = "0123456789abcdef"
let uppercase_hex_map = "0123456789ABCDEF"

let encode_into ?(uppercase = false) source source_off bytes bytes_off len =
  let hex_map = if uppercase then uppercase_hex_map else lowercase_hex_map in

  for i = 0 to len - 1 do
    let char_code = int_of_char source.[i + source_off] in

    Bytes.set bytes ((i * 2) + bytes_off) hex_map.[char_code lsr 4];
    Bytes.set bytes ((i * 2) + bytes_off + 1) hex_map.[char_code land 0x0F]
  done

let encode_substring_to_buffer ?(uppercase = false) source off len buffer =
  let hex_map = if uppercase then uppercase_hex_map else lowercase_hex_map in

  for i = 0 to len - 1 do
    let char_code = int_of_char source.[i + off] in

    Buffer.add_char buffer hex_map.[char_code lsr 4];
    Buffer.add_char buffer hex_map.[char_code land 0x0F]
  done

let encode_to_buffer ?uppercase source buffer =
  encode_substring_to_buffer ?uppercase source 0 (String.length source) buffer

let encode_substring ?uppercase source off len =
  let bytes = Bytes.create (len * 2) in
  encode_into ?uppercase source off bytes 0 len;
  Bytes.unsafe_to_string bytes

let encode ?uppercase source =
  encode_substring ?uppercase source 0 (String.length source)

let int_of_hex_digit ch =
  let char_code = int_of_char ch in

  match ch with
  | '0' .. '9' -> char_code - 0x30
  | 'A' .. 'F' -> char_code - 0x41 + 10
  | 'a' .. 'f' -> char_code - 0x61 + 10
  | ch -> invalid_arg Printf.(sprintf "bad character %C" ch)

let iter_by_hex_pair f hex off len =
  let rec loop i off len =
    if i < len then begin
      f i hex.[i + off] hex.[succ i + off];
      loop (i + 2) off len
    end
  in
  loop 0 off len

let decode_into hex hex_off bytes bytes_off len =
  if len mod 2 <> 0 then invalid_arg "leftover byte in hex string";

  iter_by_hex_pair
    begin fun i hi lo ->
      (int_of_hex_digit hi lsl 4) lor int_of_hex_digit lo
      |> Bytes.set_uint8 bytes ((i / 2) + bytes_off)
    end
    hex hex_off len

let decode_substring_to_buffer hex off len buffer =
  if len mod 2 <> 0 then invalid_arg "leftover byte in hex string";

  iter_by_hex_pair
    begin fun _ hi lo ->
      (int_of_hex_digit hi lsl 4) lor int_of_hex_digit lo
      |> Buffer.add_uint8 buffer
    end
    hex off len

let decode_to_buffer hex buffer =
  decode_substring_to_buffer hex 0 (String.length hex) buffer

let decode_substring hex off len =
  let bytes = Bytes.create (String.length hex / 2) in
  decode_into hex off bytes 0 len;
  Bytes.unsafe_to_string bytes

let decode hex = decode_substring hex 0 (String.length hex)
