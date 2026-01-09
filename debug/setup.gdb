#tui enable
tui layout regs
define bases
  set $heap_base = $s11
  set $stack_top = $sp
end

define heap
  set $x = $heap_base
  while $x <= $s11
    x/xg $x
    set $x = $x + 8
  end
end

define stack
  set $x = $sp
  while $x <= $sp
    x/xg $x
    set $x = $x + 8
  end
end

set $FIXNUM_MASK = 3
set $FIXNUM_TAG = 0
set $FIXNUM_SHIFT = 2

set $BOOLEAN_MASK = 0b10111111
set $BOOLEAN_TAG = 0b00101111
set $FALSE_VALUE = 0b00101111
set $TRUE_VALUE = 0b01101111

set $CHAR_MASK = 0b11111111
set $CHAR_TAG = 0b00001111
set $CHAR_SHIFT = 8

set $VOID_VALUE = 0b00011111

set $NULL_VALUE = 0b00111111

set $PAIR_TAG = 0b001
set $VECTOR_TAG = 0b010
set $STRING_TAG = 0b011
set $SYMBOL_TAG = 0b111
set $RATNUM_TAG = 0b101
set $CLOSURE_TAG = 0b110
set $PTR_MASK = 0b111

set $ADDRESS_MASK = 0xfffffffffffffff8
set $WORDSIZE = 8

define print_vector
  set $vector_size = $arg0[0]
  printf "#("
  set $ii = 0
  while $ii < $vector_size
    if $ii != 0 && $i != $vector_size
      printf " "
    end
    $arg0 += $WORDSIZE
    print_ptr $arg0[$ii]
    set $ii = $ii + 1
  end
  printf ")"
end

define print_string
  set $string_size = $arg0[0]
  set $string_bytes = (char *)((long *)$arg0 + 1)
  printf "\""
  set $ii = 0
  while $ii < $string_size
    printf "%c", $string_bytes[$ii]
    set $ii = $ii + 1
  end
  printf "\""
end

define print_symbol
  set $symbol_str = *((long *)$arg0)
  set $str_addr = (long *)($symbol_str & $ADDRESS_MASK)
  set $string_size = $str_addr[0]
  set $string_bytes = (char *)($str_addr + 1)
  set $ii = 0
  while $ii < $string_size
    printf "%c", $string_bytes[$ii]
    set $ii = $ii + 1
  end
end

define print_ratnum
  set $ratnum_num = $arg0[0]
  set $ratnum_den = $arg0[1]
  printf "%d/%d", $ratnum_num >> $FIXNUM_SHIFT, $ratnum_den >> $FIXNUM_SHIFT
end

define print_pair
  set $pair_car = $arg0[0]
  set $pair_cdr = $arg0[1]
  printf "("
  print_ptr $pair_car
  print_cdr $pair_cdr
  printf ")"
end

define print_cdr
  if $arg0 == $NULL_VALUE
    # null terminates the list, print nothing
  else
    if (($arg0 & $PTR_MASK) == $PAIR_TAG)
      printf " "
      set $cdr_car = *((long *)($arg0 & $ADDRESS_MASK))
      print_ptr $cdr_car
      set $cdr_cdr = *(((long *)($arg0 & $ADDRESS_MASK)) + 1)
      print_cdr $cdr_cdr
    else
      printf " . "
      print_ptr $arg0
    end
  end
end

define print_ptr
  set $ptr = (ptr_t)($arg0)
  if $ptr == $NULL_VALUE
    printf "()"
  else
    if $ptr == $FALSE_VALUE
      printf "#f"
    else
      if $ptr == $TRUE_VALUE
        printf "#t"
      else
        if $ptr == $VOID_VALUE
          printf "#<void>"
        else
          if (($ptr & $FIXNUM_MASK) == $FIXNUM_TAG)
            printf "%d", $arg0 >> $FIXNUM_SHIFT
          else
            if (($ptr & $CHAR_MASK) == $CHAR_TAG)
              printf "#\\%c", ((char) ($arg0 >> $CHAR_SHIFT))
            else
              set $arg_ptr_addr = ((long *)($arg0 & $ADDRESS_MASK))
              set $arg_ptr_tag = ($arg0 & $PTR_MASK)
              printf "ptr_addr = 0x%04x, ptr_tag = %u\n", $arg_ptr_addr, $arg_ptr_tag
              if (($arg0 & $PTR_MASK) == $VECTOR_TAG)
                print_vector $arg_ptr_addr
              else
                print "not vector"
                if ($arg_ptr_tag == $PAIR_TAG)
                  print_pair $arg_ptr_addr
                else
                  print "not pair"
                  if ($arg_ptr_tag == $STRING_TAG)
                    print_string $arg_ptr_addr
                  else
                    print "not string"
                    if ($arg_ptr_tag == $SYMBOL_TAG)
                      print_symbol $arg_ptr_addr
                    else
                      print "not symbol"
                      if ($arg_ptr_tag == $RATNUM_TAG)
                        print_ratnum $arg_ptr_addr
                      else
                        printf "Unknown value 0x%04x\n", $arg0
                      end
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
  end
end

define pp
  print_ptr $arg0
  printf "\n"
end
