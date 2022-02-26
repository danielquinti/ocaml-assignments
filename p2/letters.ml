let uppercase = function x->if ((int_of_char(x)>96)&&(int_of_char(x)<123)) then char_of_int(int_of_char(x)-32) else x;;
let lowercase = function x->if ((int_of_char(x)>64)&&(int_of_char(x)<91)) then char_of_int(int_of_char(x)+32) else x;;
