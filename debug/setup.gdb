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

set $NULL_VALUE = 0b00111111

set $VECTOR_TAG = 0b010

set $PAIR_TAG = 0b001
set $ADDRESS_MASK = 0xfffffffffffffff8
set $WORDSIZE = 8

define print_vector
  set $vector_size = *((long *)$arg0)
  printf "#("
  set $ii = 0;
  while $ii < $vector_size
    if $ii != 0 && $i != $vector_size
      printf " "
    end
    $arg0 += $WORDSIZE
    print_ptr $arg0
  end
  printf ")"
end

define print_ptr
  if $arg0 == $NULL_VALUE
    printf "()"
  else
    if $arg0 == $FALSE_VALUE
      printf "#f"
    else
      if $arg0 == $TRUE_VALUE
        printf "#t"
      else
        if (($arg0 & $FIXNUM_MASK) == $FIXNUM_TAG)
          printf "%d", $arg0 >> $FIXNUM_SHIFT
        else
          if (($arg0 & $CHAR_MASK) == $CHAR_TAG)
            printf "#\\%c", ((char) $arg0 >> CHAR_SHIFT)
          else
            if ($arg0 & $VECTOR_TAG)
              print_vector ($arg0 & $ADDRESS_MASK)
            else
              if ($arg0 & $PAIR_TAG)
                print_pair ($arg0 & $ADDRESS_MASK)
              else
                printf "Uknown $arg0ue 0x%04x\n", $arg0
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
