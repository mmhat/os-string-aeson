# os-string-aeson

This package provides the necessary functions and types to decode/encode
[OsString](https://hackage.haskell.org/package/os-string/docs/System-OsString.html#t:OsString)s
from/to a JSON value using the
[aeson](https://hackage.haskell.org/package/aeson) library.

Defining a `FromJSON OsString` or `ToJSON OsString` instance is not as
unambiguous as -- for example -- one for `FilePath`: Since the latter is a
Unicode string there is a one-to-one correspondence to JSON strings, which are
UTF-8 encoded [1]. In contrast to that, an `OsString` is an arbitrary bytestring
with no encoding information attached.

A consequence of that fact is that the instances for `OsString` involve design
decisions based on the use case. Those are summarized in the blog post
"The ultimate guide to Haskell Strings" by Julian Ospald, the author of the
`os-string` library [2]:

> 1. Convert to String (e.g. by assuming UTF-8 or UTF-16), use the existing
>    ToJSON instance and hope the receiver knows how to interpret the data.
> 2. If you're dealing with binary data, you can convert to e.g. base64 String
>    or Text and then again use the existing instances (there’s the
>    base64-bytestring-type library that does this via a newtype).
> 3. Convert the byte sequence to [Word8], which has a valid instance as well.

Following that, this library provides

  * the `fromText/toText` family of functions and a newtype wrapper `As ('Text
    Unicode) OsString` suitable to implement solution (1).

  * the `fromBase64/toBase64` family of functions and a newtype wrapper `As
    'Base64 OsString` suitable to implement solution (2).

  * the `fromBinary/toBinary` family of functions and a newtype wrapper `As
    'Binary OsString` suitable to implement solution (3).

In addition to that, a _tagged_ version of the encodings above is supported:
That is, instead of e.g. decoding from or encoding to a simple JSON string, an
object like

```json
{
    "platform": "Posix",
    "data": "some OsString here"
}
```

is used to encode additional information to the `OsString` in question.

## Usage

The package includes the same API for `OsString`, `PosixString` and
`WindowsString`:

  * `Data.OsString.Aeson` provides the API for
    [OsString](https://hackage.haskell.org/package/os-string/docs/System-OsString.html#t:OsString)
    .
  * `Data.OsString.Aeson.Posix` provides the API for
    [PosixString](https://hackage.haskell.org/package/os-string/docs/System-OsString-Posix.html#t:PosixString)
    .
  * `Data.OsString.Aeson.Windows` provides the API for
    [WindowsString](https://hackage.haskell.org/package/os-string/docs/System-OsString-Windows.html#t:WindowsString)
    .

### Examples

The examples for the different approaches can be found in the in the
[`examples`](https://github.com/mmhat/os-string-aeson/tree/main/examples)
directory:

  * [`Functions.hs`](https://github.com/mmhat/os-string-aeson/blob/main/examples/Functions.hs)
    demonstrates how to use functions to construct decoding/encoding functions
    manually.

  * [`HKD1.hs`](https://github.com/mmhat/os-string-aeson/blob/main/examples/HKD1.hs)
    demonstrates how to use the newtype wrappers provided by this library
    together with the
    [Higher-Kinded Data](https://reasonablypolymorphic.com/blog/higher-kinded-data/)
    pattern (HKD) using `Functor`s.

  * [`HKD2.hs`](https://github.com/mmhat/os-string-aeson/blob/main/examples/HKD1.hs)
    also demonstrates how to use the newtype wrappers in a HKD setting, but
    uses a type family to determine the representation of the datatype's
    fields.

## Compatibility

The support window of this library is 3 years.
That means there should be a build plan for:

  * Any _major_ GHC version published within the last 3 years.
  * Any version of a dependency published within the last 3 years.

[1]: https://datatracker.ietf.org/doc/html/rfc8259#section-8.1
[2]: https://hasufell.github.io/posts/2024-05-07-ultimate-string-guide.html#to-json
