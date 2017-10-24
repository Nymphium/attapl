# 10/19/2017
## invertible syntax
parserとprinterのギャップを埋める

### parser
string -> exp

### printer
exp -> string

## 実装
applicative styleでやっていく
```ocaml
intE (* int expression *) <$> number (* to number *)
```

`<!`で持ち上げるが､tagless finalによる制約がある

infixl, infixrでプリンタにおける演算子の優先順位をうまくやり､演算子の追加も簡単になる｡
