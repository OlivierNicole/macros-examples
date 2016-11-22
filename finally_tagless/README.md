Directly adapted from http://okmij.org/ftp/tagless-final/JFP.pdf.

Splices as shown by the compiler:
```ocaml
splice #0:
4782969
splice #1:
fun x_1  ->
             ((fun x_2  ->
                 let rec self_3 n_4 =
                   (fun x_5  ->
                      if Pervasives(*global*).(<=) x_5 0
                      then 1
                      else
                        Pervasives(*global*).( * ) x_2
                          (self_3 (Pervasives(*global*).(+) x_5 (-1)))) n_4
                    in
                 self_3) x_1) 7
splice #2:
fun x_6  ->
             Pervasives(*global*).( * ) x_6
               (Pervasives(*global*).( * ) x_6
                  (Pervasives(*global*).( * ) x_6
                     (Pervasives(*global*).( * ) x_6
                        (Pervasives(*global*).( * ) x_6
                           (Pervasives(*global*).( * ) x_6 x_6)))))
```
