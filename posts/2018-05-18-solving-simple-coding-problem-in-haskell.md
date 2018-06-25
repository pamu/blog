---
  title: Solving a coding task in Haskell
  author: Nagarjuna Pamu
---

One of my friends who recently gave a online coding test shared a simple programming task which he encountered. Though he solved the problem correctly his solution was timing out for large inputs. Lets see the problem and give a better solution and analyze the problem more.


### __Problem statement__

There are `n` candies to consume to start with. Only `a` candies can be consumed at once. When `a` candies are consumed `b` candies are generated. User tries to consume all the candies. The question is weather its possible to consume all the candies. If possible How many candies are consumed by the user until he finishes all the candies. If number of candies left is less than `a` then all candies can be consumed and no generation happens. Generation of candies happens only when `a` candies are consumed.



For example:

1. There are `3` candies to start with `n = 3`

2. Number of candies person will be able to consume at once is `2` i.e `a = 2`.

3. Number of candies that get generated when `2` candies are consumed is `1` i.e `b = 1`

Now, number of candies will the person consumes when initial count is 3, consumption rate is 2 at once and generation rate is 1.


- *Step 1:*

    - _Initial count = 3_
    - _Consumed at once = 2_
    - _Generated = 1_
    - _Left = 3 - 2 + 1 = 2_
    - _Total consumed = 2_


- *Step 2:*

    - _Initial count = 2 (left over in step 1)_
    - _Consumed at once = 2 (always consumed is two if 2 or more than 2 are available)_
    - _Generated = 1 (generated is always 1)_
    - _Left = 2 - 2 + 1 = 1_
    - _Total consumed = 2 + 2 = 4_


- Step 3:

  _As left over is less than 2 (consumed at once) no candy is generated._

    - _Initial count = 1_
    - _Consumed at once = 1 (consumed is 1 as only 1 candy is left)_
    - _Generated = 0 (no generation happens)_
    - _Left = 1 - 1 = 0_
    - _Total consumed = 2 + 2 + 1 = 5_


The answer is `5`. That means if starting candies count is 3. If 2 candies are consumed at once while generating 1 candy for every 2 candies consumption. Then in total person might have consumed `5` candies.


### __Lets write a program to compute the same.__


Lets create method which simulates eating `a` number of candies only and then gives out left overs. For this lets create a record which stores left over candies and consumed candies

```haskell
data Result = Result
  { left :: Integer -- represents left over candies
  , ate :: Integer -- represents consumed candies
  } deriving (Show)
```

_Now, after single pass ie, after consuming `a` number of candies `b` number of candies get generated_

``` haskell
eatOnce :: Integer -> Integer -> Integer -> Result
eatOnce initialCount generated oneTime -- oneTime is nothing but `a` value
  | initialCount < oneTime = Result {left = 0, ate = initialCount}
  | initialCount == oneTime = Result {left = 1, ate = initialCount}
  | initialCount > oneTime = Result {left = initialCount - oneTime + generated, ate = oneTime}
```

_Then, repeat `eatOnce` until all the candies are completed._

```haskell
candiesConsumed :: Integer -> Integer -> Integer
candiesConsumed a b = candiesConsumedHelper a b 0
  where
    candiesConsumedHelper :: Integer -> Integer -> Integer -> Integer
    candiesConsumedHelper amount oneTime consumed
      | amount == 0 = consumed
      | otherwise =
        let result = eatOnce amount oneTime
        in candiesConsumedHelper (left result) oneTime (consumed + ate result)
```

_when `amount` becomes zero return the consumed value else keep eating using `eatOnce`_

### _Putting everything in one program_


_Candies.hs_

```haskell
import Debug.Trace (trace)

main :: IO ()
main = do
  let result = candiesConsumed 1346 10
  putStrLn $ "result " ++ show result
  return ()

data Result = Result
  { left :: Integer
  , ate :: Integer
  } deriving (Show)

eatOnce :: Integer -> Integer -> Result
eatOnce existing oneTime
  | existing < oneTime = Result {left = 0, ate = existing}
  | existing == oneTime = Result {left = 1, ate = existing}
  | otherwise = Result {left = existing - oneTime + 1, ate = oneTime}

candiesConsumed :: Integer -> Integer -> Integer
candiesConsumed a b = candiesConsumedHelper a b 0
  where
    candiesConsumedHelper :: Integer -> Integer -> Integer -> Integer
    candiesConsumedHelper amount oneTime consumed
      | amount == 0 = consumed
      | otherwise =
        let result = eatOnce amount oneTime
            newConsumed =
              candiesConsumedHelper
                (left result)
                oneTime
                (consumed + ate result)
        in trace
             ("step: amount: " ++ show amount ++ " consumed: " ++ show consumed)
             newConsumed

```

_See `trace`, used in the code for debugging purpose, to know the state of variable while the program is executing_

_Here is how trace looks like_

```haskell
  *Main> :t trace
  trace :: String -> a -> a
```

_`trace` is very helpful to know the state of the program while running. If simple `print` or `putStrLn` is used, it will introduce `IO` and program loses the simplicity very quickly._

__Warning:__
__`trace` must not be used in production code. Its for debugging purposes only.__

### _Lets now run the program for various input sizes_

```haskell
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
Prelude> :set +s
Prelude> :load Candies.hs
[1 of 1] Compiling Main             ( Candies.hs, interpreted )
Ok, one module loaded.
(0.06 secs,)
*Main> candiesConsumed 5 3
step: amount: 5 consumed: 0
step: amount: 3 consumed: 3
step: amount: 1 consumed: 6
7
(0.01 secs, 93,232 bytes)
*Main> candiesConsumed 3 2
step: amount: 3 consumed: 0
step: amount: 2 consumed: 2
step: amount: 1 consumed: 4
5
(0.00 secs, 92,856 bytes)
*Main> candiesConsumed 13123231 10
step: amount: 13123231 consumed: 0
step: amount: 13123222 consumed: 10
step: amount: 13123213 consumed: 20
step: amount: 13123204 consumed: 30
-- Very large output
```

From the third input its clear that this solution takes too long for larger inputs.

For input `candiesConsumed 2 1` program never exits. Why?

```haskell
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
Prelude> :set +s
Prelude> :load Candies.hs
[1 of 1] Compiling Main             ( Candies.hs, interpreted )
Ok, one module loaded.
(0.06 secs,)
*Main> candiesConsumed 2 1
-- program never exits
```

_Here the consumption rate is `1` and also production rate is also `1`. So, amount of candies will never become 0._

__Production must be always be less than consumption for this program to exit.__


### _Making the candies consumption faster_

_This method can be made faster by division method._

_For example:_

1. _Initial amount = 6_
2. _Consumption = 3 and generation = 1_
3. _5 can be represented as 3 + 2 + 1_
4. _3 consumed, 1 generated => In 3 + 2 + 1 replace 3 with 1, now its becomes 1 + 2 + 1_
5. _Write 1 + 2 + 1 in terms of 3 and then again repeat the process of replacing 3 with 1_
6. _3 + 1 when consumed 3 generated 1, sum becomes 1 + 1_
7. _As 2 is less than 3 .. 2 will be consumed._
8. _In total 3 + 3 + 2 = 8_


_That means,_

1. `newConsumed = consumed + (div amount consumption) * consumption`
2. `newAmount = amount + (div amount consumption) * generated`

_Expressing above formuals in Haskell_

_FasterCandies.hs_

```haskell
import Debug.Trace (trace)

candies :: Integer -> Integer -> Integer
candies a b = candiesHelper a b 1 0
  where
    candiesHelper :: Integer -> Integer -> Integer -> Integer -> Integer
    candiesHelper amount oneTime generated consumed
      | oneTime < generated =
        error
          "consumption must be greater than generation for candies to converge."
      | amount == 0 = consumed
      | amount <= oneTime = amount + consumed
      | otherwise =
        let multiples = div amount oneTime
            newAmount = multiples * generated + (amount - multiples * oneTime)
            newConsumed = multiples * oneTime
        in candiesHelper newAmount oneTime generated (consumed + newConsumed)
```

_Trying on REPL_

```haskell
*Main> :load FasterCandies.hs
[1 of 1] Compiling Main             ( FasterCandies.hs, interpreted )
Ok, one module loaded.
*Main> candies 4 3
5
*Main> candies 3 2
5
*Main> candies 7 3
10
*Main> :set +s
*Main> candies 32232 10
35813
(0.00 secs, 71,992 bytes)
*Main> candies 322343223422 10
358159137135
(0.00 secs, 79,600 bytes)
```

_Now, the code is quite faster than the previous one. Also the case in which consumption is lesser than or equal to production is patched._
