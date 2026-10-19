let lowercase_hex_map = "0123456789abcdef"
let uppercase_hex_map = "0123456789ABCDEF"
let required_length_for_encode len = len * 2
let required_length_for_decode len = len / 2

let check_source_range s off len =
  (* Range checks. Negatives are checked before the upper bound so that the
   subtraction [length - off] cannot wrap around on very large [len]. *)
  if off < 0 || len < 0 || off > String.length s - len then
    invalid_arg "source range out of bounds"

let check_hex_range s off len =
  if off < 0 || len < 0 || off > String.length s - len then
    invalid_arg "hex range out of bounds"

let check_destination_range b off len =
  if off < 0 || len < 0 || off > Bytes.length b - len then
    invalid_arg "destination buffer too small"

let encode_into ?(uppercase = false) source source_off bytes bytes_off len =
  check_source_range source source_off len;
  check_destination_range bytes bytes_off (required_length_for_encode len);

  let hex_map = if uppercase then uppercase_hex_map else lowercase_hex_map in

  for i = 0 to len - 1 do
    let char_code = int_of_char source.[i + source_off] in
    Bytes.set bytes ((i * 2) + bytes_off) hex_map.[char_code lsr 4];
    Bytes.set bytes ((i * 2) + bytes_off + 1) hex_map.[char_code land 0x0F]
  done

let encode_substring_to_buffer ?(uppercase = false) source off len buffer =
  check_source_range source off len;

  let hex_map = if uppercase then uppercase_hex_map else lowercase_hex_map in

  for i = 0 to len - 1 do
    let char_code = int_of_char source.[i + off] in
    Buffer.add_char buffer hex_map.[char_code lsr 4];
    Buffer.add_char buffer hex_map.[char_code land 0x0F]
  done

let encode_to_buffer ?uppercase source buffer =
  encode_substring_to_buffer ?uppercase source 0 (String.length source) buffer

let encode_substring ?uppercase source off len =
  check_source_range source off len;
  let bytes = Bytes.create (required_length_for_encode len) in
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
  let rec loop i =
    if i < len then begin
      f i hex.[i + off] hex.[succ i + off];
      loop (i + 2)
    end
  in
  loop 0

let decode_into hex hex_off bytes bytes_off len =
  check_hex_range hex hex_off len;
  if len mod 2 <> 0 then invalid_arg "leftover byte in hex string";
  check_destination_range bytes bytes_off (required_length_for_decode len);

  iter_by_hex_pair
    begin fun i hi lo ->
      (int_of_hex_digit hi lsl 4) lor int_of_hex_digit lo
      |> Bytes.set_uint8 bytes ((i / 2) + bytes_off)
    end
    hex hex_off len

let decode_substring_to_buffer hex off len buffer =
  check_hex_range hex off len;
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
  check_hex_range hex off len;
  if len mod 2 <> 0 then invalid_arg "leftover byte in hex string";

  let bytes = Bytes.create (required_length_for_decode len) in
  decode_into hex off bytes 0 len;
  Bytes.unsafe_to_string bytes

let decode hex = decode_substring hex 0 (String.length hex)
