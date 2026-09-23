# assets ― quarto revealjs の小さな検証ファイル

本番デッキを触らずに挙動を確かめるための最小再現。どれもRチャンクを含まないので
数秒でレンダーできる（01.qmd は WDI API を叩くため約5分かかる）。

## インデント

* `indent_test.qmd` / `indent_test.html` / `indent.png`
* 4通りを並べて比較した結果（2026-09-15 実測）
  * `::: {style="margin-left: 3em"}` … 一度きりならこれ。CSS不要
  * `::: {.myindent}` + CSS … 繰り返すならクラスに名前を付ける。`my.css` に用意済み
  * `>` … 引用ブロック。左罫線と灰色文字が付く
  * ネストしたリスト項目 … 箇条書きの点が付く
* qmd 本文に直接 `<style>` を書いてもHTMLに残る（この例で使用）

## フラグメントの出現順

* `frag_test.qmd` / `frag_probe.js` / `fragorder2.png`
* 問題: `incremental: true` はリスト項目だけをフラグメント化する。素の段落は
  クラスが付かず、step 0 で最初から表示されてしまう
* 対処: `::: {.fragment}` で段落を包む。番号を振らなければ本文の順に発火する
* `frag_probe.js` の使い方: レンダー済みHTMLの**最後の** `</body>` の直前に挿入する
  （reveal の同梱JSが `</body>` という文字列を含むため、最初の一致に入れると壊れる）。
  `Reveal.slide(1,0,0)` で目的のスライドへ移動してから `Reveal.next()` で1段ずつ進め、
  各段階で `visible` クラスの付いたフラグメントを記録して画面に出す
* 実測結果: 箇条書き4つ → 原因は不明 → 開発経済学では未解明 の順（fragorder2.png）

## 注意

reveal のフラグメント番号を URL ハッシュ（`#/84/0/9`）で指定しても効かない。
`fragmentInURL` が有効でないため、どの番号でも step 0 のスクリーンショットになる。
必ず上記の `Reveal.slide()` 方式を使うこと。

## スライドの文字サイズ

* `deckB-extra.css` ― `lec_slides/2026/deckB-extra.css` の写し
* `.smaller` は実測で約 0.664em（Deng のスライドを 0.80em にしたら 776→935 に伸び、
  比 1.205 から逆算）。それより大きくしたい段階として3つ用意した
  * `.mid` 0.76em / `.mid2` 0.70em / `.mid3` 0.90em
* 各クラスに表用の打ち消しを必ず対で書く。本文だけ大きくし、表は元の見た目を保つため
  * 例: `.mid` なら `0.664 / 0.76 = 0.874` を表に掛け戻す
* `.sublist` は入れ子リスト1箇所だけを小さくする用。スライド全体の入れ子に
  CSSを当てると同じスライドの他の入れ子や他のスライドまで縮むため

### 使い方

スライド見出しにクラスを付け、YAMLの `css:` にファイルを並べる。

```
## 見出し {.nonincremental .mid2}
```

```
format:
  revealjs:
    css: [../../style/hiragino.scss, deckB-extra.css]
```

### 落とし穴

* qmd本文に ```` ```{=html} ```` で囲んだ `<style>` を置いても quarto に捨てられる。
  HTMLに一切残らない。`.css` ファイルを `css:` に渡すこと
* `incremental: true` のスライドにコードチャンクを入れると、`.nonincremental` が
  効かなくなり全リストが step 0 で消える。全スライドが `.nonincremental` なら
  YAMLで `incremental: false` にするのが確実

## my.css

* `my.css` ― 使い回す字下げクラス。`.myindent` (3em) と `.myindent2` (6em)
* 動作確認済み: `mycss_test.qmd` → `mycss_test.html` / `mycss.png`（2026-09-15 実測）
  * 外部ファイルとして `css: [my.css]` で読み込み、両方のルールがHTMLに入ることと
    実際に字下げされることをスクリーンショットで確認
* 使い方

```
format:
  revealjs:
    css: [my.css]
```

```
::: {.myindent}
本文
:::
```

* 別フォルダのqmdから使うときは相対パスで指す（例: `css: [../../assets/my.css]`）

## my.css に取り込んだ定義の実測（2026-09-15）

`mycss_test.qmd` に各定義を使う例を書き、`css_probe.js` で
`getComputedStyle` を読んで確かめた（cssprobe.png / cssprobe2.png）。

### 効いているもの

| 定義 | 実測値 |
|---|---|
| `Red` `Orange` `Blue` `Gray` | `<Red>文字</Red>` と書けば効く。rgb(255,0,0) / rgb(233,78,44) / rgb(0,0,255) / rgb(128,128,128) |
| `.description-list` | font-size 20px（親の50%）、dt/dd とも display: inline |
| `.description-lb` | font-size 28px（親の70%）、dt は block、dd の margin-bottom 28px |
| `.ListArrow` | arrow-row は display: flex、左45% / 矢印 text-align:center / 右50% |
| `.reveal .slides pre code` | max-height 800px |
| `.myindent` | margin-left 120px（3em） |

### 効いていないもの

* **`Red` などをクラスとして使うと効かない。** `<span class="Red">` は既定色
  rgb(34,34,34) のまま。セレクタが要素名 `Red` であってクラス `.Red` ではないため。
  クラスとしても使いたいなら `.Red, Red { color: red; }` のように両方書く
* **`#TOC { font-color: ... }` は無効。** `font-color` というCSSプロパティは存在せず、
  ブラウザが宣言ごと捨てる（`t.style.cssText = 'font-color: red'` → 長さ0で確認）。
  色を変えるなら `color:`。なお revealjs に `#TOC` は無く、HTML文書用の指定

### 取り込み時に1点だけ直した箇所

元の定義に `#### description with a line break between item header and texts` という
行があった。これはmarkdownの見出しでCSSでは構文エラーになるため、`/* */` の
コメントに変えて取り込んだ。
