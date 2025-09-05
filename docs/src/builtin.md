# Built-In Definitions

```par
/// Nat

dec Nat.Add   : [Nat, Nat] Nat
dec Nat.Mul   : [Nat, Nat] Nat
dec Nat.Div   : [Nat, Nat] Nat
dec Nat.Mod   : [Nat, Nat] Nat
dec Nat.Min   : [Nat, Nat] Nat
dec Nat.Max   : [Nat, Int] Nat
dec Nat.Clamp : [Int] [Nat, Nat] Nat

dec Nat.Equals  : [Nat, Nat] Bool
dec Nat.Compare : [Nat, Nat] Ordering

dec Nat.Repeat : [Nat] recursive either {
  .end!,
  .step self,
}

dec Nat.RepeatLazy : [Nat] recursive either {
  .end!,
  .step box choice {
    .next => self,
  }
}

dec Nat.Range : [Nat, Nat] List<Nat>

dec Nat.ToString : [Nat] String

dec Nat.FromString : [String] either {
  .ok Nat,
  .err!,
}


/// Int

dec Int.Add   : [Int, Int] Int
dec Int.Sub   : [Int, Int] Int
dec Int.Mul   : [Int, Int] Int
dec Int.Div   : [Int, Int] Int
dec Int.Mod   : [Int, Nat] Nat
dec Int.Min   : [Int, Int] Int
dec Int.Max   : [Int, Int] Int
dec Int.Abs   : [Int] Nat
dec Int.Clamp : [Int] [Int, Int] Int

dec Int.Equals  : [Int, Int] Bool
dec Int.Compare : [Int, Int] Ordering

dec Int.Range : [Int, Int] List<Int>

dec Int.ToString : [Int] String

dec Int.FromString : [String] either {
  .ok Int,
  .err!,
}


/// Bool

type Bool = either {
  .false!,
  .true!,
}


/// Result

type Result<e, a> = either {
  .ok a,
  .err e,
}

dec Result.Always : [type a] [Result<either {}, a>] a
def Result.Always = [type a] [result] result.case {
  .ok value => value,
  .err impossible => impossible.case {},
}


/// List

type List<a> = recursive either {
  .end!,
  .item(a) self,
}

type List.Builder<a> = iterative choice {
  .add(a) => self,
  .build => List<a>,
}

dec List.Builder : [type a] List.Builder<a>
def List.Builder = [type a]
  let append: [List<a>] List<a> = [xs] xs
  in begin case {
    .add(x) =>
      let append: [List<a>] List<a>
        = [xs] append(.item(x) xs)
      in loop,
    .build => append(.end!),
  }


/// Ordering

type Ordering = either {
  .less!,
  .equal!,
  .greater!,
}


/// Char

type Char.Class = either {
  .any!,
  .char Char,
  .whitespace!,
  .ascii either {
    .any!,
    .alpha!,
    .alphanum!,
    .digit!,
  },
}

dec Char.Equals : [Char, Char] Bool
dec Char.Code   : [Char] Nat
dec Char.Is     : [Char, Char.Class] Bool


/// String

type String.Builder = iterative choice {
  .add(String) => self,
  .build => String,
}

type String.Parser<errIn, errOut> = recursive iterative/attempt choice {
  .close(Result<errIn, !>) => Result<errOut, !>,
  .remainder => Result<errOut, String>,
  .char => either {
    .end Result<errOut, !>,
    .char(Char) self,
  },
  .match(String.Pattern, String.Pattern) => either {
    .end Result<errOut, !>,
    .fail self/attempt,
    .match(String, String) self,
  },
  .matchEnd(String.Pattern, String.Pattern) => either {
    .end Result<errOut, !>,
    .fail self/attempt,
    .match(String, String)!,
  },
}

type String.Pattern = recursive either {
  .empty!,
  .str String,
  .one Char.Class,
  .non Char.Class,
  .min Nat,
  .max Nat,
  .repeat self,
  .repeat1 self,
  .concat List<self>,
  .and List<self>,
  .or List<self>,
}

dec String.Quote     : [String] String
dec String.FromBytes : [Bytes] String

dec String.Builder : String.Builder
dec String.Parse   : [String] String.Parser<either{}, either{}>

dec String.ParseReader : [type errIn, errOut] [Bytes.Reader<errIn, errOut>] String.Parser<errIn, errOut>


/// Byte

type Byte.Class = either {
  .any!,
  .byte Byte,
  .range(Byte, Byte)!,
}

dec Byte.Equals : [Byte, Byte] Bool
dec Byte.Code   : [Byte] Nat
dec Byte.Is     : [Byte, Byte.Class] Bool


/// Bytes

type Bytes.Builder = iterative choice {
  .add(Bytes) => self,
  .build => Bytes,
}

type Bytes.Reader<errIn, errOut> = recursive choice {
  .close(Result<errIn, !>) => Result<errOut, !>,
  .read => Result<errOut, either {
    .end!,
    .chunk(Bytes) self,
  }>,
}

type Bytes.Writer<errIn, errOut> = iterative choice {
  .close(Result<errIn, !>) => Result<errOut, !>,
  .flush => Result<errOut, self>,
  .write(Bytes) => Result<errOut, self>,
  .writeString(String) => Result<errOut, self>,
}

type Bytes.Parser<errIn, errOut> = recursive iterative/attempt choice {
  .close(Result<errIn, !>) => Result<errOut, !>,
  .remainder => Result<errOut, Bytes>,
  .byte => either {
    .end Result<errOut, !>,
    .byte(Byte) self,
  },
  .match(Bytes.Pattern, Bytes.Pattern) => either {
    .end Result<errOut, !>,
    .fail self/attempt,
    .match(Bytes, Bytes) self,
  },
  .matchEnd(Bytes.Pattern, Bytes.Pattern) => either {
    .end Result<errOut, !>,
    .fail self/attempt,
    .match(Bytes, Bytes)!,
  },
}

type Bytes.Pattern = recursive either {
  .empty!,
  .bytes Bytes,
  .one Byte.Class,
  .non Byte.Class,
  .min Nat,
  .max Nat,
  .repeat self,
  .repeat1 self,
  .concat List<self>,
  .and List<self>,
  .or List<self>,
}

dec Bytes.FromString : [String] Bytes

dec Bytes.Builder : Bytes.Builder
dec Bytes.Parse   : [Bytes] Bytes.Parser<either{}, either{}>

dec Bytes.ParseReader : [type errIn, errOut] [Bytes.Reader<errIn, errOut>] Bytes.Parser<errIn, errOut>


/// Console

type Console = iterative choice {
  .close => !,
  .print(String) => self,
  .prompt(String) => (Result<!, String>) self,
}

dec Console.Open : Console


/// Os

type Os.Error  = String
type Os.Reader = Bytes.Reader<either{}, Os.Error>
type Os.Writer = Bytes.Writer<either{}, Os.Error>

type Os.Path = iterative/append recursive/parent box choice {
  .stringName => String,
  .bytesName => Bytes,
  .stringAbsolute => String,
  .bytesAbsolute => Bytes,
  .stringParts => List<String>,
  .bytesParts => List<Bytes>,

  .parent => Result<!, self/parent>,
  .appendString(String) => self/append,
  .appendBytes(Bytes) => self/append,

  .openFile => Result<Os.Error, Os.Reader>,
  .createOrReplaceFile => Result<Os.Error, Os.Writer>,
  .createNewFile => Result<Os.Error, Os.Writer>,
  .appendToFile => Result<Os.Error, Os.Writer>,
  .createOrAppendToFile => Result<Os.Error, Os.Writer>,

  .listDir => Result<Os.Error, List<self/append>>,
  .traverseDir => Result<Os.Error, recursive/tree either {
    .end!,
    .file(self/append) self/tree,
    .dir(self/append, self/tree) self/tree,
  }>,
  .createDir => Result<Os.Error, !>,
}

dec Os.PathFromString : [String] Os.Path
dec Os.PathFromBytes  : [Bytes] Os.Path

dec Os.Stdin  : Os.Reader
dec Os.Stdout : Os.Writer


/// Http

type Http.Error  = String
type Http.Reader = Bytes.Reader<either{}, Http.Error>
type Http.Writer = Bytes.Writer<either{}, Http.Error>

dec Http.Request :
  [String, String, List<(String) String>]
  [[Http.Writer] Result<Http.Error, !>]
  Result<Http.Error, (Nat, List<(String) String>) Http.Reader>


/// Map

type Map<k, v> = iterative choice {
  .list => List<(k) v>,

  .put(k, v) => (Result<v, !>) self,
  .delete(k) => (Result<!, v>) self,

  .get(k) => (Result<!, v>) choice {
    .put(v) => self,
    .delete => self,
  },

  .getOr(k, box v) => (v) choice {
    .put(v) => self,
    .delete => self,
  },
}

dec Map.String : [type v] [List<(String) box v>] Map<String, v>
dec Map.Int    : [type v] [List<(Int) box v>]    Map<Int, v>
dec Map.Nat    : [type v] [List<(Nat) box v>]    Map<Nat, v>


/// Cell (EXPERIMENTAL)

type Cell<a> = iterative choice {
  .end => ?,
  .split(dual self) => self,
  .take => (a) choice {
    .put(a) => self,
  }
}

dec Cell.Share : [type a] [a, dual Cell<a>] a


/// Debug

dec Debug.Log : [String] !
```
