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
  .addChar(Char) => self,
  .add(String) => self,
  .build => String,
}

type String.Reader<e> = recursive iterative/attempt choice {
  .close => !,
  .remainder => Result<e, String>,
  .char => either {
    .end Result<e, !>,
    .char(Char) self,
  },
  .match(String.Pattern, String.Pattern) => either {
    .end Result<e, !>,
    .fail self/attempt,
    .match(String, String) self,
  },
  .matchEnd(String.Pattern, String.Pattern) => either {
    .end Result<e, !>,
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

dec String.Builder : String.Builder
dec String.Reader  : [String] String.Reader<either {}>

dec String.Quote : [String] String

/// Byte

dec Byte.Equals : [Byte, Byte] Bool
dec Byte.Code   : [Byte] Nat

/// Bytes

type Bytes.Builder = iterative choice {
  .add(Bytes) => self,
  .build => Bytes,
}

/// Console

type Console = iterative choice {
  .close => !,
  .print(String) => self,
  .prompt(String) => (Result<!, String>) self,
}

dec Console.Open : Console

/// Storage

type Storage.Error = String

type Storage.PathInfo = box choice {
  .name => String,
  .absolute => String,
}

type Storage.FileInfo = box choice {
  .path => Storage.PathInfo,
  .size => Nat.Nat,
  .readUTF8 => Result<Storage.Error, String.Reader<Storage.Error>>,
}

type Storage.DirInfo = recursive box choice {
  .path => Storage.PathInfo,
  .list => Result<Storage.Error, List<either {
    .file Storage.FileInfo,
    .dir self,
  }>>,
}

dec Storage.Get : [String] Result<Storage.Error, either {
  .file Storage.FileInfo,
  .dir Storage.DirInfo,
}>

/// Debug

dec Debug.Log : [String] !
```
