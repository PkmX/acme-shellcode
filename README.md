# acme-shellcode

Embed shellcode in your Haskell code with Template Haskell!

## Example

```
λ> import Acme.Shellcode
λ> [shellQQ| 0x48 0x31 0xd2 0x48 0xbb 0x2f 0x2f 0x62 0x69 0x6e 0x2f 0x73 0x68 0x48 0xc1 0xeb 0x08 0x53 0x48 0x89 0xe7 0x50 0x57 0x48 0x89 0xe6 0xb0 0x3b 0x0f 0x05 |]
sh-4.3$
```
