# The Big Table

## Expression syntax

<table>

<tr>
<td><strong>Type</strong></td>
<td><strong>Construction</strong></td>
<td><strong>Destruction</strong></td>
</tr>

<tr>
<td><pre><code class="language-par">type Unit = !</code></pre></td>
<td><pre><code class="language-par">let value = !</code></pre></td>
<td><pre><code class="language-par">let ! = value</code></pre></td>
</tr>

<tr/>

<tr>
<td><pre><code class="language-par">type Either = either {
  .left String,
  .right Int,
}</code></pre></td>
<td><pre><code class="language-par">let value: Either = .left "Hello!"</code></pre></td>
<td><pre><code class="language-par">let result = value.case {
  .left str => String.Length(str),
  .right num => Int.Mul(num, 5),
}</code></pre></td>
</tr>

<tr/>

<tr>
<td><pre><code class="language-par">type Pair = (String) Int</code></pre></td>
<td><pre><code class="language-par">let value = ("Hello!") 42</code></pre></td>
<td><pre><code class="language-par">let (str) num = value</code></pre></td>
</tr>

<tr/>

<tr>
<td><pre><code class="language-par">type Function = [Int] String</code></pre></td>
<td><pre><code class="language-par">let value = [num: Int] Int.ToString(num)</code></pre></td>
<td><pre><code class="language-par">let str = value(42)</code></pre></td>
</tr>

<tr/>

<tr>
<td><pre><code class="language-par">type Choice = choice {
  .left => String,
  .right => Int,
}</code></pre></td>
<td><pre><code class="language-par">let value: Choice = case {
  .left => "Hello!",
  .right => 42,
}</code></pre></td>
<td><pre><code class="language-par">let num = value.right</code></pre></td>
</tr>

<tr/>

<tr>
<td><pre><code class="language-par">type Continuation = ?</code></pre></td>
<td><em>No expression syntax</em></td>
<td><em>No expression syntax</em></td>
</tr>

</table>

## Process syntax

<table>

<tr>
<td><strong>Type</strong></td>
<td><strong>Construction</strong></td>
<td><strong>Destruction</strong></td>
</tr>

<tr>
<td><pre><code class="language-par">type Unit = !</code></pre></td>
<td><pre><code class="language-par">let value = chan c {
  c!
}</code></pre></td>
<td><pre><code class="language-par">value?</code></pre></td>
</tr>

<tr/>

<tr>
<td><pre><code class="language-par">type Either = either {
  .left String,
  .right Int,
}</code></pre></td>
<td><pre><code class="language-par">let value: Either = chan c {
  c.left
  c <> "Hello!"
}</code></pre></td>
<td><pre><code class="language-par">value.case {
  .left => {
    let result = String.Length(value)
  }
  .right => {
    let result = Int.Mul(value, 5)
  }
}
// `result` is in scope here
</code></pre></td>
</tr>

<tr/>

<tr>
<td><pre><code class="language-par">type Pair = (String) Int</code></pre></td>
<td><pre><code class="language-par">let value = chan c {
  c("Hello!")
  c <> 42
}</code></pre></td>
<td><pre><code class="language-par">value[str]
let num = value</code></pre></td>
</tr>

<tr/>

<tr>
<td><pre><code class="language-par">type Function = [Int] String</code></pre></td>
<td><pre><code class="language-par">let value = chan c {
  c[num: Int]
  c <> Int.ToString(num)
}</code></pre></td>
<td><pre><code class="language-par">value(42)
let result = value</code></pre></td>
</tr>

<tr/>

<tr>
<td><pre><code class="language-par">type Choice = choice {
  .left => String,
  .right => Int,
}</code></pre></td>
<td><pre><code class="language-par">let value = chan c {
  c.case {
    .left  => { c <> "Hello!" }
    .right => { c <> 42 }
  }
}</code></pre></td>
<td><pre><code class="language-par">value.right
let num = value</code></pre></td>
</tr>

<tr/>

<tr>
<td><pre><code class="language-par">type Continuation = ?</code></pre></td>
<td><pre><code class="language-par">let outer: ! = chan break {
  let value: ? = chan c {
    c?     // construction
    break!
  }
  value!   // destruction
}</code></pre></td>
<td><em>Shown on the left</em></td>
</tr>

</table>
