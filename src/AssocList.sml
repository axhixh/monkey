structure AssocList =
struct
  type ('key, 'value) T = ('key * 'value) list

  fun add (key, value) list = (key, value) :: list

  (* find the value associated with the key. finds the first instance
   *  of the key so the most recent addition.
   *  returns NONE if it isn't present in the list
   *)
  fun find key [] = NONE
    | find key ((k, v) :: rest) =
        if key = k then SOME v else find key rest

  (* deletes all occurrences of key from the association list *)
  fun delete key [] = []
    | delete key ((k, v) :: rest) =
        if key = k then delete key rest else (k, v) :: delete key rest

  (* update the value of the most recent instance of the key.
  * adds the value if it isn't present
  *)
  fun update key value [] = [(key, value)]
    | update key value ((k, v) :: rest) =
        if key = k then (key, value) :: rest
        else (k, v) :: update key value rest
end
