# 構文
- ==, /=
- func x y を x `func` y　と書ける
- べき乗計算は^が使える
- [1..10], [1,3..10]
- [x|x<-[1..10],odd x]
- func::Int->Int->Int
  - func x y = x + y
- hoge :: a -> a 
  - 型変数
  - func::Num a => a->a->a
  - fromIntegral::(Num b, Integral a) => a->b  
- read "5" ::Int
- パターンマッチ
  - fact 0 = 1
  - fact n = n * fact(n-1))
  - リストパターンマッチ
    - head' (x:_) = x
    - [x,y] (x:y:[])
  - asパターン
    - firstLetter all@(x:xs)
  - ガード
  　 - max' a b
        | a <= b    = b
        | otherwise = a
  - where
     - where bmi = weight / height^2
  - let式
     - (let a = 9; b = 1 in a + b)
# 演算子
- !! 添え字アクセス
  - [1,2,3] !! 1 => 2
# 関数
### 演算
- div 整数除算(/だと小数になる)
- mod (%はない？)

### リスト操作
- head, tail
- init, last
- take
- drop
- maximum
- minimum
- sum
- product
- elem
- zip
- replicate
  - replicate 3 10 => [10, 10, 10]

### タプル操作
- fst
- snd

# 型クラス
- Eq
  - ==, /=
- Ord
  - >, <, >=, <=
- Show
  - show
- Read
  - read
- Enum
  - [1..10]
  - succ, pred
- Bounded
  - minBound, maxBound
- Num
  - Int, Integer, Float, Double
- Floating
  - Float, Double  
- Integral
  - Int, Integer    
  - fromIntegral