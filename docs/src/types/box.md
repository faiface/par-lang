# Box

Par has a **linear type system.** By default, values must be used **exactly once.**

But not all values need that kind of discipline. Sometimes, you want to:
- Pass a function around multiple times.
- Discard an unused value.
- Compose higher-order utilities freely.

That’s where **box types** come in.

## Non-linear types — even without `box`

Even without box types, some types in Par are already non-linear. These are the **data types:**
- [Unit](./unit.md)
- [Either](./either.md)
- [Pair](./pair.md)
- [Recursive](./recursive.md)
- All the [primitives](../structure/primitive_types.md): `Int`, `Nat`, `String`, `Char`.

Any combination of these is always non-linear — they can be copied and discarded freely.

But types that contain [**functions,**](./function.md) [**choices,**](./choice.md) and other non-data types
are linear — no matter how deeply nested.

Now, consider this: what if you want to **apply a function to each element in a list?**

A plain function in Par is linear — it can only be used once. So applying it repeatedly requires a workaround.

## Reusable functions, the hard way

Without `box`, we can build reusable functions by encoding a usage protocol manually:

```par
type Mapper<a, b> = iterative choice {
  .close => !,
  .apply(a) => (b) self,
}
```

This protocol gives us:
- `.apply` to use the function.
- `.close` to clean up.

Here's a `Map` function that uses it:

```par
dec Map : [type a, b] [List<a>, Mapper<a, b>] List<b>
def Map = [type a, b] [list, mapper] list.begin.case {
  .end! => let ! = mapper.close in .end!,
  .item(x) xs => let (x1) mapper = mapper.apply(x) in .item(x1) xs.loop,
}
```

And using it:

```par
def NumberStrings = Map(type Int, String)(Int.Range(1, 100), begin case {
  .close => !,
  .apply(n) => (Int.ToString(n)) loop,
})
```

This works — but it’s verbose.

Every reusable function needs to be manually encoded with a protocol like `Mapper`.
Copying, closing, and chaining all become manual work.

## Box types to the rescue

Instead of encoding reusability into the type manually, Par lets you `box` a value.

A `box T` is a non-linear version of **any** type `T`. You can:

- **Copy** a `box T`.
- **Drop** a `box T`.
- Pass it around freely.

You can construct boxed values using:

```par
box <expression>
```

This constructs a value of type `box T`, where `T` is the type of the expression.

The only rule is: **You can only capture non-linear variables in a `box` expression.**

That includes:
- Data types (`Int`, `String`, `List<Int>`, etc.)
- Other `box` values.

The word _**capture**_ here refers to _using local variables_ inside the expression that were
_created outside of that expression._

## A better `Map`

With `box`, we can rewrite the `Map` function much more cleanly:

```par
dec Map : [type a, b] [List<a>, box [a] b] List<b>
def Map = [type a, b] [list, f] list.begin.case {
  .end! => .end!,
  .item(x) xs => .item(f(x)) xs.loop,
}
```

Let’s try it out:

```par
def NumberStrings = Map(type Int, String)(
  Int.Range(1, 100),
  box Int.ToString,
)
```

No wrappers, no manual protocols. The boxed function can be used freely, because the `box` type makes
it non-linear. This is exactly what `box` was made for.

## Subtyping

Boxed types fit naturally into Par's subtyping.

**A `box T` can be used anywhere a `T` is expected.**

```par
def BoxInt: box Int = 42       // OK: Int is non-linear
def UseInt: Int = BoxInt       // OK: box Int can be used as Int
```

And **if `T` is already non-linear, then `T` can be used anywhere a `box T` is expected.**

```par
def Boxes: List<box Int> = *(1, 2, 3)
def Ints: List<Int> = Boxes
```

**For non-linear types, `T` and `box T` are effectively interchangeable.**

## Another example: Filtering a list

Let’s write a function that filters a list using a boxed predicate.

```par
dec Filter : [type a] [List<box a>, box [a] Bool] List<a>

def Filter = [type a] [list, predicate] list.begin.case {
  .end! => .end!,
  .item(x) xs => predicate(x).case {
    .true! => .item(x) xs.loop,
    .false! => xs.loop,
  }
}
```

Note the types:
- We accept a list of `box a`, because elements might be discarded.
- But we return a `List<a>` — not a `List<box a>`.

Let’s try it out:

```par
def Evens = Filter(type Int)(
  Int.Range(1, 100),
  box [n] Int.Equals(0, Int.Mod(n, 2)),
)
```

Here:
- `Int.Range(1, 100)` gives a `List<Int>`, which is valid because `Int` can be used as a `box Int`.
- The result is inferred as `List<Int>`, not `List<box Int>`.

This avoids bloating the result type with redundant boxes — keeping things clear.

And if we were filtering a list of boxed values already?

```par
Filter(type box T)(listOfBoxT, predicate)
```

That works too. Thanks to subtyping, everything lines up as you'd expect.