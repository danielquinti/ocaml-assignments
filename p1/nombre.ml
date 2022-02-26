let print_string s =
    for i=0 to String.length s-1 do
        print_char s.[i]
    done;;

let println s=
    print_string s;
    print_newline();;

println "Quintillán Quintillán, Daniel";;
println "daniel.quintillan@udc.es";;
