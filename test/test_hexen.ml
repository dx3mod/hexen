[@@@ai_generated]

(* ==================== encode ==================== *)
module Test_encode = struct
  let test_empty_string () =
    Alcotest.(check string "empty" "" (Hexen.encode ""))

  let test_simple_lowercase () =
    Alcotest.(check string "hello" "68656c6c6f" (Hexen.encode "hello"))

  let test_simple_uppercase () =
    Alcotest.(
      check string "HELLO" "68656C6C6F" (Hexen.encode ~uppercase:true "hello"))

  let test_uppercase_only_affects_letters () =
    Alcotest.(
      check string "hex digits" "ABCDEF"
        (Hexen.encode ~uppercase:true "\xab\xcd\xef"));
    Alcotest.(
      check string "digits unchanged" "0123456789"
        (Hexen.encode ~uppercase:true "\x01\x23\x45\x67\x89"))

  let test_binary_data () =
    Alcotest.(
      check string "binary" "0001027f80ff"
        (Hexen.encode "\x00\x01\x02\x7f\x80\xff"))

  let test_single_null_byte () =
    Alcotest.(check string "null" "00" (Hexen.encode "\x00"))

  let test_single_byte_ff () =
    Alcotest.(check string "ff" "ff" (Hexen.encode "\xff"))

  let test_output_length_is_2x_input () =
    let s = "abcdef" in
    Alcotest.(
      check int "2x" (2 * String.length s) (String.length (Hexen.encode s)))
end

(* ==================== encode_substring ==================== *)
module Test_encode_substring = struct
  let test_middle_slice () =
    Alcotest.(
      check string "substring" "68656c6c6f"
        (Hexen.encode_substring "XXhelloYY" 2 5))

  let test_uppercase () =
    Alcotest.(
      check string "substring upper" "68656C6C6F"
        (Hexen.encode_substring ~uppercase:true "XXhelloYY" 2 5))

  let test_zero_length () =
    Alcotest.(check string "zero len" "" (Hexen.encode_substring "hello" 2 0))

  let test_whole_string () =
    Alcotest.(
      check string "whole" "68656c6c6f" (Hexen.encode_substring "hello" 0 5))

  let test_prefix_slice () =
    Alcotest.(
      check string "prefix" "68656c" (Hexen.encode_substring "hello" 0 3))

  let test_suffix_slice () =
    Alcotest.(
      check string "suffix" "6c6c6f" (Hexen.encode_substring "hello" 2 3))

  let test_single_byte () =
    Alcotest.(check string "single" "6c" (Hexen.encode_substring "hello" 2 1))

  let test_negative_offset () =
    Alcotest.(
      check_raises "neg off" (Invalid_argument "source range out of bounds")
        (fun () -> ignore (Hexen.encode_substring "hello" (-1) 3)))

  let test_negative_length () =
    Alcotest.(
      check_raises "neg len" (Invalid_argument "source range out of bounds")
        (fun () -> ignore (Hexen.encode_substring "hello" 0 (-1))))

  let test_offset_plus_length_exceeds () =
    Alcotest.(
      check_raises "too long" (Invalid_argument "source range out of bounds")
        (fun () -> ignore (Hexen.encode_substring "hello" 3 5)))
end

(* ==================== encode_to_buffer ==================== *)
module Test_encode_to_buffer = struct
  let test_empty_into_empty_buffer () =
    let buf = Buffer.create 16 in
    Hexen.encode_to_buffer buf "";
    Alcotest.(check string "empty" "" (Buffer.contents buf))

  let test_simple () =
    let buf = Buffer.create 16 in
    Hexen.encode_to_buffer buf "hi";
    Alcotest.(check string "hi" "6869" (Buffer.contents buf))

  let test_uppercase () =
    let buf = Buffer.create 16 in
    Hexen.encode_to_buffer ~uppercase:true buf "hi";
    Alcotest.(check string "HI" "6869" (Buffer.contents buf))

  let test_appends_to_existing_content () =
    let buf = Buffer.create 16 in
    Buffer.add_string buf "prefix:";
    Hexen.encode_to_buffer buf "hi";
    Alcotest.(check string "appended" "prefix:6869" (Buffer.contents buf))

  let test_two_calls_append () =
    let buf = Buffer.create 16 in
    Hexen.encode_to_buffer buf "a";
    Hexen.encode_to_buffer buf "b";
    Alcotest.(check string "two calls" "6162" (Buffer.contents buf))
end

(* ==================== encode_substring_to_buffer ==================== *)
module Test_encode_substring_to_buffer = struct
  let test_middle_slice () =
    let buf = Buffer.create 16 in
    Hexen.encode_substring_to_buffer buf "XXhelloYY" 2 5;
    Alcotest.(check string "substring" "68656c6c6f" (Buffer.contents buf))

  let test_uppercase () =
    let buf = Buffer.create 16 in
    Hexen.encode_substring_to_buffer ~uppercase:true buf "hello" 0 5;
    Alcotest.(check string "upper" "68656C6C6F" (Buffer.contents buf))

  let test_zero_length () =
    let buf = Buffer.create 16 in
    Buffer.add_string buf "keep";
    Hexen.encode_substring_to_buffer buf "hello" 2 0;
    Alcotest.(check string "no change" "keep" (Buffer.contents buf))

  let test_appends_to_existing_content () =
    let buf = Buffer.create 16 in
    Buffer.add_string buf "> ";
    Hexen.encode_substring_to_buffer buf "XXabcYY" 2 3;
    Alcotest.(check string "appended" "> 616263" (Buffer.contents buf))

  let test_negative_offset () =
    let buf = Buffer.create 16 in
    Alcotest.(
      check_raises "neg off" (Invalid_argument "source range out of bounds")
        (fun () -> Hexen.encode_substring_to_buffer buf "hello" (-1) 3))

  let test_negative_length () =
    let buf = Buffer.create 16 in
    Alcotest.(
      check_raises "neg len" (Invalid_argument "source range out of bounds")
        (fun () -> Hexen.encode_substring_to_buffer buf "hello" 0 (-1)))

  let test_offset_plus_length_exceeds () =
    let buf = Buffer.create 16 in
    Alcotest.(
      check_raises "too long" (Invalid_argument "source range out of bounds")
        (fun () -> Hexen.encode_substring_to_buffer buf "hello" 3 5))

  let test_buffer_untouched_on_error () =
    let buf = Buffer.create 16 in
    Buffer.add_string buf "keep";
    (try Hexen.encode_substring_to_buffer buf "hello" (-1) 3
     with Invalid_argument _ -> ());
    Alcotest.(check string "buffer untouched" "keep" (Buffer.contents buf))
end

(* ==================== encode_into ==================== *)
module Test_encode_into = struct
  let test_simple () =
    let dst = Bytes.create 10 in
    Hexen.encode_into "hello" 0 dst 0 5;
    Alcotest.(check string "hello" "68656c6c6f" (Bytes.to_string dst))

  let test_uppercase () =
    let dst = Bytes.create 10 in
    Hexen.encode_into ~uppercase:true "hello" 0 dst 0 5;
    Alcotest.(check string "HELLO" "68656C6C6F" (Bytes.to_string dst))

  let test_zero_length () =
    let dst = Bytes.make 4 'X' in
    Hexen.encode_into "hello" 2 dst 0 0;
    Alcotest.(check string "unchanged" "XXXX" (Bytes.to_string dst))

  let test_destination_offset () =
    let dst = Bytes.make 8 'X' in
    Hexen.encode_into "hi" 0 dst 2 2;
    Alcotest.(check string "with offset" "XX6869XX" (Bytes.to_string dst))

  let test_source_offset () =
    let dst = Bytes.create 6 in
    Hexen.encode_into "XXabc" 2 dst 0 3;
    Alcotest.(check string "src offset" "616263" (Bytes.to_string dst))

  let test_both_offsets () =
    let dst = Bytes.make 10 '-' in
    Hexen.encode_into "!!!abc!!!" 3 dst 2 3;
    Alcotest.(check string "both" "--616263--" (Bytes.to_string dst))

  let test_single_byte () =
    let dst = Bytes.create 2 in
    Hexen.encode_into "XaX" 1 dst 0 1;
    Alcotest.(check string "one byte" "61" (Bytes.to_string dst))

  let test_negative_source_offset () =
    let dst = Bytes.make 20 'X' in
    Alcotest.(
      check_raises "neg src off" (Invalid_argument "source range out of bounds")
        (fun () -> Hexen.encode_into "hello" (-1) dst 0 3))

  let test_negative_length () =
    let dst = Bytes.make 20 'X' in
    Alcotest.(
      check_raises "neg len" (Invalid_argument "source range out of bounds")
        (fun () -> Hexen.encode_into "hello" 0 dst 0 (-1)))

  let test_source_too_short () =
    let dst = Bytes.make 20 'X' in
    Alcotest.(
      check_raises "src too short"
        (Invalid_argument "source range out of bounds") (fun () ->
          Hexen.encode_into "hi" 0 dst 0 3))

  let test_destination_too_small () =
    let dst = Bytes.make 4 'X' in
    Alcotest.(
      check_raises "dst too small"
        (Invalid_argument "destination buffer too small") (fun () ->
          Hexen.encode_into "hello" 0 dst 0 5))

  let test_destination_negative_offset () =
    let dst = Bytes.make 20 'X' in
    Alcotest.(
      check_raises "neg dst off"
        (Invalid_argument "destination buffer too small") (fun () ->
          Hexen.encode_into "hello" 0 dst (-1) 5))

  let test_source_error_wins_over_destination_error () =
    let dst = Bytes.make 4 'X' in
    Alcotest.(
      check_raises "src error first"
        (Invalid_argument "source range out of bounds") (fun () ->
          Hexen.encode_into "hi" 0 dst 0 3))

  let test_destination_untouched_on_error () =
    let dst = Bytes.make 4 'X' in
    (try Hexen.encode_into "hello" 0 dst 0 5 with Invalid_argument _ -> ());
    Alcotest.(check string "dst untouched" "XXXX" (Bytes.to_string dst))
end

(* ==================== decode ==================== *)
module Test_decode = struct
  let test_empty_string () =
    Alcotest.(check string "empty" "" (Hexen.decode ""))

  let test_lowercase_hex () =
    Alcotest.(check string "hello" "hello" (Hexen.decode "68656c6c6f"))

  let test_uppercase_hex () =
    Alcotest.(check string "hello" "hello" (Hexen.decode "68656C6C6F"))

  let test_mixed_case_hex () =
    Alcotest.(check string "hello" "hello" (Hexen.decode "68656C6c6F"))

  let test_binary_data () =
    Alcotest.(
      check string "binary" "\x00\x01\x02\x7f\x80\xff"
        (Hexen.decode "0001027f80ff"))

  let test_single_null_byte () =
    Alcotest.(check string "null" "\x00" (Hexen.decode "00"))

  let test_invalid_character_z () =
    Alcotest.(
      check_raises "bad char" (Invalid_argument "bad character 'z'") (fun () ->
          ignore (Hexen.decode "7a7z")))

  let test_invalid_character_G () =
    Alcotest.(
      check_raises "bad char" (Invalid_argument "bad character 'G'") (fun () ->
          ignore (Hexen.decode "7G")))

  let test_invalid_character_space () =
    Alcotest.(
      check_raises "bad char" (Invalid_argument "bad character ' '") (fun () ->
          ignore (Hexen.decode "7 ")))

  let test_odd_length_3_chars () =
    Alcotest.(
      check_raises "odd length" (Invalid_argument "leftover byte in hex string")
        (fun () -> ignore (Hexen.decode "abc")))

  let test_odd_length_1_char () =
    Alcotest.(
      check_raises "odd length" (Invalid_argument "leftover byte in hex string")
        (fun () -> ignore (Hexen.decode "a")))
end

(* ==================== decode_substring ==================== *)
module Test_decode_substring = struct
  let test_whole_string () =
    Alcotest.(
      check string "whole" "hello" (Hexen.decode_substring "68656c6c6f" 0 10))

  let test_uppercase_whole () =
    Alcotest.(
      check string "upper" "hello" (Hexen.decode_substring "68656C6C6F" 0 10))

  let test_middle_slice_with_prefix_suffix () =
    Alcotest.(
      check string "middle" "hello"
        (Hexen.decode_substring "XX68656c6c6fYY" 2 10))

  let test_prefix_slice () =
    Alcotest.(
      check string "prefix" "hel" (Hexen.decode_substring "68656c6c6f" 0 6))

  let test_suffix_slice () =
    Alcotest.(
      check string "suffix" "llo" (Hexen.decode_substring "68656c6c6f" 4 6))

  let test_zero_length () =
    Alcotest.(check string "zero" "" (Hexen.decode_substring "68656c6c6f" 0 0))

  let test_odd_length () =
    Alcotest.(
      check_raises "odd" (Invalid_argument "leftover byte in hex string")
        (fun () -> ignore (Hexen.decode_substring "abc" 0 3)))

  let test_invalid_character () =
    Alcotest.(
      check_raises "bad char" (Invalid_argument "bad character 'z'") (fun () ->
          ignore (Hexen.decode_substring "6z" 0 2)))

  let test_negative_offset () =
    Alcotest.(
      check_raises "neg off" (Invalid_argument "hex range out of bounds")
        (fun () -> ignore (Hexen.decode_substring "68656c6c6f" (-1) 4)))

  let test_negative_length () =
    Alcotest.(
      check_raises "neg len" (Invalid_argument "hex range out of bounds")
        (fun () -> ignore (Hexen.decode_substring "68656c6c6f" 0 (-2))))

  let test_offset_plus_length_exceeds () =
    Alcotest.(
      check_raises "too long" (Invalid_argument "hex range out of bounds")
        (fun () -> ignore (Hexen.decode_substring "68656c6c6f" 5 10)))

  let test_range_error_wins_over_odd_length () =
    Alcotest.(
      check_raises "range first" (Invalid_argument "hex range out of bounds")
        (fun () -> ignore (Hexen.decode_substring "abc" 0 5)))
end

(* ==================== decode_to_buffer ==================== *)
module Test_decode_to_buffer = struct
  let test_empty_into_empty_buffer () =
    let buf = Buffer.create 16 in
    Hexen.decode_to_buffer buf "";
    Alcotest.(check string "empty" "" (Buffer.contents buf))

  let test_simple () =
    let buf = Buffer.create 16 in
    Hexen.decode_to_buffer buf "68656c6c6f";
    Alcotest.(check string "hello" "hello" (Buffer.contents buf))

  let test_appends_to_existing_content () =
    let buf = Buffer.create 16 in
    Buffer.add_string buf "prefix:";
    Hexen.decode_to_buffer buf "68656c6c6f";
    Alcotest.(check string "appended" "prefix:hello" (Buffer.contents buf))

  let test_two_calls_append () =
    let buf = Buffer.create 16 in
    Hexen.decode_to_buffer buf "61";
    Hexen.decode_to_buffer buf "62";
    Alcotest.(check string "two calls" "ab" (Buffer.contents buf))

  let test_odd_length () =
    let buf = Buffer.create 16 in
    Alcotest.(
      check_raises "odd" (Invalid_argument "leftover byte in hex string")
        (fun () -> Hexen.decode_to_buffer buf "abc"))

  let test_invalid_character () =
    let buf = Buffer.create 16 in
    Alcotest.(
      check_raises "bad char" (Invalid_argument "bad character 'z'") (fun () ->
          Hexen.decode_to_buffer buf "7z"))
end

(* ==================== decode_substring_to_buffer ==================== *)
module Test_decode_substring_to_buffer = struct
  let test_middle_slice () =
    let buf = Buffer.create 16 in
    Hexen.decode_substring_to_buffer buf "XX68656c6c6fYY" 2 10;
    Alcotest.(check string "hello" "hello" (Buffer.contents buf))

  let test_prefix_slice () =
    let buf = Buffer.create 16 in
    Hexen.decode_substring_to_buffer buf "68656c6c6f" 0 6;
    Alcotest.(check string "hel" "hel" (Buffer.contents buf))

  let test_suffix_slice () =
    let buf = Buffer.create 16 in
    Hexen.decode_substring_to_buffer buf "68656c6c6f" 4 6;
    Alcotest.(check string "llo" "llo" (Buffer.contents buf))

  let test_zero_length () =
    let buf = Buffer.create 16 in
    Buffer.add_string buf "keep";
    Hexen.decode_substring_to_buffer buf "68656c6c6f" 0 0;
    Alcotest.(check string "no change" "keep" (Buffer.contents buf))

  let test_appends_to_existing_content () =
    let buf = Buffer.create 16 in
    Buffer.add_string buf "> ";
    Hexen.decode_substring_to_buffer buf "616263" 0 6;
    Alcotest.(check string "appended" "> abc" (Buffer.contents buf))

  let test_odd_length () =
    let buf = Buffer.create 16 in
    Alcotest.(
      check_raises "odd" (Invalid_argument "leftover byte in hex string")
        (fun () -> Hexen.decode_substring_to_buffer buf "abc" 0 3))

  let test_invalid_character () =
    let buf = Buffer.create 16 in
    Alcotest.(
      check_raises "bad char" (Invalid_argument "bad character 'z'") (fun () ->
          Hexen.decode_substring_to_buffer buf "7z" 0 2))

  let test_negative_offset () =
    let buf = Buffer.create 16 in
    Alcotest.(
      check_raises "neg off" (Invalid_argument "hex range out of bounds")
        (fun () -> Hexen.decode_substring_to_buffer buf "68656c6c6f" (-1) 4))

  let test_negative_length () =
    let buf = Buffer.create 16 in
    Alcotest.(
      check_raises "neg len" (Invalid_argument "hex range out of bounds")
        (fun () -> Hexen.decode_substring_to_buffer buf "68656c6c6f" 0 (-2)))

  let test_offset_plus_length_exceeds () =
    let buf = Buffer.create 16 in
    Alcotest.(
      check_raises "too long" (Invalid_argument "hex range out of bounds")
        (fun () -> Hexen.decode_substring_to_buffer buf "68656c6c6f" 5 10))

  let test_buffer_untouched_on_error () =
    let buf = Buffer.create 16 in
    Buffer.add_string buf "keep";
    (try Hexen.decode_substring_to_buffer buf "68656c6c6f" (-1) 4
     with Invalid_argument _ -> ());
    Alcotest.(check string "buffer untouched" "keep" (Buffer.contents buf))
end

(* ==================== decode_into ==================== *)
module Test_decode_into = struct
  let test_simple () =
    let dst = Bytes.create 5 in
    Hexen.decode_into "68656c6c6f" 0 dst 0 10;
    Alcotest.(check string "hello" "hello" (Bytes.to_string dst))

  let test_uppercase_source () =
    let dst = Bytes.create 5 in
    Hexen.decode_into "68656C6C6F" 0 dst 0 10;
    Alcotest.(check string "hello" "hello" (Bytes.to_string dst))

  let test_destination_offset () =
    let dst = Bytes.make 7 'X' in
    Hexen.decode_into "68656c6c6f" 0 dst 1 10;
    Alcotest.(check string "XhelloX" "XhelloX" (Bytes.to_string dst))

  let test_source_offset () =
    let dst = Bytes.create 5 in
    Hexen.decode_into "XX68656c6c6fYY" 2 dst 0 10;
    Alcotest.(check string "hello" "hello" (Bytes.to_string dst))

  let test_both_offsets () =
    let dst = Bytes.make 7 '-' in
    Hexen.decode_into "!!616263!!" 2 dst 2 6;
    Alcotest.(check string "abc" "--abc--" (Bytes.to_string dst))

  let test_single_byte () =
    let dst = Bytes.create 1 in
    Hexen.decode_into "61" 0 dst 0 2;
    Alcotest.(check string "a" "a" (Bytes.to_string dst))

  let test_zero_length () =
    let dst = Bytes.make 4 'X' in
    Hexen.decode_into "68656c6c6f" 0 dst 0 0;
    Alcotest.(check string "unchanged" "XXXX" (Bytes.to_string dst))

  let test_odd_length () =
    let dst = Bytes.create 8 in
    Alcotest.(
      check_raises "odd" (Invalid_argument "leftover byte in hex string")
        (fun () -> Hexen.decode_into "abc" 0 dst 0 3))

  let test_negative_hex_offset () =
    let dst = Bytes.make 10 'X' in
    Alcotest.(
      check_raises "neg hex off" (Invalid_argument "hex range out of bounds")
        (fun () -> Hexen.decode_into "68656c6c6f" (-1) dst 0 4))

  let test_negative_length () =
    let dst = Bytes.make 10 'X' in
    Alcotest.(
      check_raises "neg len" (Invalid_argument "hex range out of bounds")
        (fun () -> Hexen.decode_into "68656c6c6f" 0 dst 0 (-2)))

  let test_hex_too_short () =
    let dst = Bytes.make 10 'X' in
    Alcotest.(
      check_raises "hex too short" (Invalid_argument "hex range out of bounds")
        (fun () -> Hexen.decode_into "68" 0 dst 0 4))

  let test_destination_too_small () =
    let dst = Bytes.make 2 'X' in
    Alcotest.(
      check_raises "dst too small"
        (Invalid_argument "destination buffer too small") (fun () ->
          Hexen.decode_into "68656c6c6f" 0 dst 0 10))

  let test_destination_negative_offset () =
    let dst = Bytes.make 10 'X' in
    Alcotest.(
      check_raises "neg dst off"
        (Invalid_argument "destination buffer too small") (fun () ->
          Hexen.decode_into "68656c6c6f" 0 dst (-1) 10))

  let test_odd_length_wins_over_destination_error () =
    let dst = Bytes.make 1 'X' in
    Alcotest.(
      check_raises "odd first" (Invalid_argument "leftover byte in hex string")
        (fun () -> Hexen.decode_into "abc" 0 dst 0 3))

  let test_destination_untouched_on_error () =
    let dst = Bytes.make 2 'X' in
    (try Hexen.decode_into "68656c6c6f" 0 dst 0 10
     with Invalid_argument _ -> ());
    Alcotest.(check string "dst untouched" "XX" (Bytes.to_string dst))
end

(* ==================== roundtrip ==================== *)
module Test_roundtrip = struct
  let test_empty () =
    Alcotest.(check string "roundtrip" "" (Hexen.decode (Hexen.encode "")))

  let test_ascii () =
    let s = "The quick brown fox jumps over the lazy dog" in
    Alcotest.(check string "roundtrip" s (Hexen.decode (Hexen.encode s)))

  let test_all_256_byte_values () =
    let s = String.init 256 (fun i -> Char.chr i) in
    Alcotest.(check string "roundtrip" s (Hexen.decode (Hexen.encode s)))

  let test_all_256_byte_values_uppercase () =
    let s = String.init 256 (fun i -> Char.chr i) in
    Alcotest.(
      check string "roundtrip" s (Hexen.decode (Hexen.encode ~uppercase:true s)))

  let test_encode_is_lowercase_by_default () =
    let s = String.init 256 (fun i -> Char.chr i) in
    let enc = Hexen.encode s in
    let has_upper =
      let r = ref false in
      String.iter (fun c -> if c >= 'A' && c <= 'F' then r := true) enc;
      !r
    in
    Alcotest.(check bool "no uppercase letters" false has_upper)

  let test_longer_random_data () =
    Random.self_init ();
    let s = String.init 10_000 (fun _ -> Char.chr (Random.int 256)) in
    Alcotest.(check string "roundtrip" s (Hexen.decode (Hexen.encode s)))
end

(* ==================== runner ==================== *)
let () =
  let open Alcotest in
  run "Hexen"
    [
      ( "encode",
        [
          test_case "empty string" `Quick Test_encode.test_empty_string;
          test_case "simple lowercase" `Quick Test_encode.test_simple_lowercase;
          test_case "simple uppercase" `Quick Test_encode.test_simple_uppercase;
          test_case "uppercase only affects letters" `Quick
            Test_encode.test_uppercase_only_affects_letters;
          test_case "binary data" `Quick Test_encode.test_binary_data;
          test_case "single null byte" `Quick Test_encode.test_single_null_byte;
          test_case "single byte 0xff" `Quick Test_encode.test_single_byte_ff;
          test_case "output length is 2x input" `Quick
            Test_encode.test_output_length_is_2x_input;
        ] );
      ( "encode_substring",
        [
          test_case "middle slice" `Quick
            Test_encode_substring.test_middle_slice;
          test_case "uppercase" `Quick Test_encode_substring.test_uppercase;
          test_case "zero length" `Quick Test_encode_substring.test_zero_length;
          test_case "whole string" `Quick
            Test_encode_substring.test_whole_string;
          test_case "prefix slice" `Quick
            Test_encode_substring.test_prefix_slice;
          test_case "suffix slice" `Quick
            Test_encode_substring.test_suffix_slice;
          test_case "single byte" `Quick Test_encode_substring.test_single_byte;
          test_case "negative offset" `Quick
            Test_encode_substring.test_negative_offset;
          test_case "negative length" `Quick
            Test_encode_substring.test_negative_length;
          test_case "offset + length exceeds source" `Quick
            Test_encode_substring.test_offset_plus_length_exceeds;
        ] );
      ( "encode_to_buffer",
        [
          test_case "empty into empty buffer" `Quick
            Test_encode_to_buffer.test_empty_into_empty_buffer;
          test_case "simple" `Quick Test_encode_to_buffer.test_simple;
          test_case "uppercase" `Quick Test_encode_to_buffer.test_uppercase;
          test_case "appends to existing content" `Quick
            Test_encode_to_buffer.test_appends_to_existing_content;
          test_case "two calls append" `Quick
            Test_encode_to_buffer.test_two_calls_append;
        ] );
      ( "encode_substring_to_buffer",
        [
          test_case "middle slice" `Quick
            Test_encode_substring_to_buffer.test_middle_slice;
          test_case "uppercase" `Quick
            Test_encode_substring_to_buffer.test_uppercase;
          test_case "zero length" `Quick
            Test_encode_substring_to_buffer.test_zero_length;
          test_case "appends to existing content" `Quick
            Test_encode_substring_to_buffer.test_appends_to_existing_content;
          test_case "negative offset" `Quick
            Test_encode_substring_to_buffer.test_negative_offset;
          test_case "negative length" `Quick
            Test_encode_substring_to_buffer.test_negative_length;
          test_case "offset + length exceeds source" `Quick
            Test_encode_substring_to_buffer.test_offset_plus_length_exceeds;
          test_case "buffer untouched on error" `Quick
            Test_encode_substring_to_buffer.test_buffer_untouched_on_error;
        ] );
      ( "encode_into",
        [
          test_case "simple" `Quick Test_encode_into.test_simple;
          test_case "uppercase" `Quick Test_encode_into.test_uppercase;
          test_case "zero length" `Quick Test_encode_into.test_zero_length;
          test_case "destination offset" `Quick
            Test_encode_into.test_destination_offset;
          test_case "source offset" `Quick Test_encode_into.test_source_offset;
          test_case "both offsets" `Quick Test_encode_into.test_both_offsets;
          test_case "single byte" `Quick Test_encode_into.test_single_byte;
          test_case "negative source offset" `Quick
            Test_encode_into.test_negative_source_offset;
          test_case "negative length" `Quick
            Test_encode_into.test_negative_length;
          test_case "source too short" `Quick
            Test_encode_into.test_source_too_short;
          test_case "destination too small" `Quick
            Test_encode_into.test_destination_too_small;
          test_case "destination negative offset" `Quick
            Test_encode_into.test_destination_negative_offset;
          test_case "source error wins over destination error" `Quick
            Test_encode_into.test_source_error_wins_over_destination_error;
          test_case "destination untouched on error" `Quick
            Test_encode_into.test_destination_untouched_on_error;
        ] );
      ( "decode",
        [
          test_case "empty string" `Quick Test_decode.test_empty_string;
          test_case "lowercase hex" `Quick Test_decode.test_lowercase_hex;
          test_case "uppercase hex" `Quick Test_decode.test_uppercase_hex;
          test_case "mixed case hex" `Quick Test_decode.test_mixed_case_hex;
          test_case "binary data" `Quick Test_decode.test_binary_data;
          test_case "single null byte" `Quick Test_decode.test_single_null_byte;
          test_case "invalid character 'z'" `Quick
            Test_decode.test_invalid_character_z;
          test_case "invalid character 'G'" `Quick
            Test_decode.test_invalid_character_G;
          test_case "invalid character ' '" `Quick
            Test_decode.test_invalid_character_space;
          test_case "odd length (3 chars)" `Quick
            Test_decode.test_odd_length_3_chars;
          test_case "odd length (1 char)" `Quick
            Test_decode.test_odd_length_1_char;
        ] );
      ( "decode_substring",
        [
          test_case "whole string" `Quick
            Test_decode_substring.test_whole_string;
          test_case "uppercase whole" `Quick
            Test_decode_substring.test_uppercase_whole;
          test_case "middle slice with prefix/suffix" `Quick
            Test_decode_substring.test_middle_slice_with_prefix_suffix;
          test_case "prefix slice" `Quick
            Test_decode_substring.test_prefix_slice;
          test_case "suffix slice" `Quick
            Test_decode_substring.test_suffix_slice;
          test_case "zero length" `Quick Test_decode_substring.test_zero_length;
          test_case "odd length" `Quick Test_decode_substring.test_odd_length;
          test_case "invalid character" `Quick
            Test_decode_substring.test_invalid_character;
          test_case "negative offset" `Quick
            Test_decode_substring.test_negative_offset;
          test_case "negative length" `Quick
            Test_decode_substring.test_negative_length;
          test_case "offset + length exceeds hex" `Quick
            Test_decode_substring.test_offset_plus_length_exceeds;
          test_case "range error wins over odd length" `Quick
            Test_decode_substring.test_range_error_wins_over_odd_length;
        ] );
      ( "decode_to_buffer",
        [
          test_case "empty into empty buffer" `Quick
            Test_decode_to_buffer.test_empty_into_empty_buffer;
          test_case "simple" `Quick Test_decode_to_buffer.test_simple;
          test_case "appends to existing content" `Quick
            Test_decode_to_buffer.test_appends_to_existing_content;
          test_case "two calls append" `Quick
            Test_decode_to_buffer.test_two_calls_append;
          test_case "odd length" `Quick Test_decode_to_buffer.test_odd_length;
          test_case "invalid character" `Quick
            Test_decode_to_buffer.test_invalid_character;
        ] );
      ( "decode_substring_to_buffer",
        [
          test_case "middle slice" `Quick
            Test_decode_substring_to_buffer.test_middle_slice;
          test_case "prefix slice" `Quick
            Test_decode_substring_to_buffer.test_prefix_slice;
          test_case "suffix slice" `Quick
            Test_decode_substring_to_buffer.test_suffix_slice;
          test_case "zero length" `Quick
            Test_decode_substring_to_buffer.test_zero_length;
          test_case "appends to existing content" `Quick
            Test_decode_substring_to_buffer.test_appends_to_existing_content;
          test_case "odd length" `Quick
            Test_decode_substring_to_buffer.test_odd_length;
          test_case "invalid character" `Quick
            Test_decode_substring_to_buffer.test_invalid_character;
          test_case "negative offset" `Quick
            Test_decode_substring_to_buffer.test_negative_offset;
          test_case "negative length" `Quick
            Test_decode_substring_to_buffer.test_negative_length;
          test_case "offset + length exceeds hex" `Quick
            Test_decode_substring_to_buffer.test_offset_plus_length_exceeds;
          test_case "buffer untouched on error" `Quick
            Test_decode_substring_to_buffer.test_buffer_untouched_on_error;
        ] );
      ( "decode_into",
        [
          test_case "simple" `Quick Test_decode_into.test_simple;
          test_case "uppercase source" `Quick
            Test_decode_into.test_uppercase_source;
          test_case "destination offset" `Quick
            Test_decode_into.test_destination_offset;
          test_case "source offset" `Quick Test_decode_into.test_source_offset;
          test_case "both offsets" `Quick Test_decode_into.test_both_offsets;
          test_case "single byte" `Quick Test_decode_into.test_single_byte;
          test_case "zero length" `Quick Test_decode_into.test_zero_length;
          test_case "odd length" `Quick Test_decode_into.test_odd_length;
          test_case "negative hex offset" `Quick
            Test_decode_into.test_negative_hex_offset;
          test_case "negative length" `Quick
            Test_decode_into.test_negative_length;
          test_case "hex too short" `Quick Test_decode_into.test_hex_too_short;
          test_case "destination too small" `Quick
            Test_decode_into.test_destination_too_small;
          test_case "destination negative offset" `Quick
            Test_decode_into.test_destination_negative_offset;
          test_case "odd length wins over destination error" `Quick
            Test_decode_into.test_odd_length_wins_over_destination_error;
          test_case "destination untouched on error" `Quick
            Test_decode_into.test_destination_untouched_on_error;
        ] );
      ( "roundtrip",
        [
          test_case "empty" `Quick Test_roundtrip.test_empty;
          test_case "ascii" `Quick Test_roundtrip.test_ascii;
          test_case "all 256 byte values" `Quick
            Test_roundtrip.test_all_256_byte_values;
          test_case "all 256 byte values uppercase" `Quick
            Test_roundtrip.test_all_256_byte_values_uppercase;
          test_case "encode is lowercase by default" `Quick
            Test_roundtrip.test_encode_is_lowercase_by_default;
          test_case "longer random data" `Quick
            Test_roundtrip.test_longer_random_data;
        ] );
    ]
