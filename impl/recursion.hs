module HFRecursion where

  -- Your naiive 2^n fib function
  fib :: Int -> Int
  fib 0 = 1
  fib 1 = 1
  fib n = (fib $ n - 1) + (fib $ n - 2)

  -- Memoized version: n runtime
  memoized_fib :: Int -> Integer
  memoized_fib = (map fib [0 ..] !!)
     where fib 0 = 0
           fib 1 = 1
           fib n = memoized_fib (n-2) + memoized_fib (n-1)
