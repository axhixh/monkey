val _ =
  let
    val list1: (string, int) AssocList.T = []
    val list2 = AssocList.add ("a", 5) list1
    val list3 = AssocList.add ("b", 6) list2
    val list4 = AssocList.update ("a", 9) list3
    val list5 = AssocList.delete "a" list4
  in
    print (if NONE = AssocList.find "a" list1 then "pass1;" else "fail1;");
    print (if SOME 5 = AssocList.find "a" list2 then "pass2;" else "fail2;");
    print (if SOME 6 = AssocList.find "b" list3 then "pass3;" else "fail3;");
    print (if SOME 9 = AssocList.find "a" list4 then "pass4;" else "fail4;");
    print (if NONE = AssocList.find "a" list5 then "pass5;" else "fail5;")
  end
