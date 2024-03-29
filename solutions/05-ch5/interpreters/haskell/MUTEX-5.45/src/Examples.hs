module Examples
  ( example1
  , example2
  , example3a, example3b
  , example4
  , example5
  ) where


import qualified Interpreter as I


-- Example 1: Two threads showing interleaved computation.
--
-- NOTE: You can reproduce the output shown in the book
--       by using maxTimeSlice = 10.
example1 :: Int -> IO I.Value
example1 maxTimeSlice =
  let
    input =
      "letrec                                          \
      \  noisy(l) =                                    \
      \    if null?(l) then                            \
      \      0                                         \
      \    else                                        \
      \      begin                                     \
      \        print(car(l));                          \
      \        (noisy cdr(l))                          \
      \      end                                       \
      \in                                              \
      \begin                                           \
      \  spawn(proc (d) (noisy list(1, 2, 3, 4, 5)));  \
      \  spawn(proc (d) (noisy list(6, 7, 8, 9, 10))); \
      \  print(100);                                   \
      \  33                                            \
      \end                                             "
  in
  I.runIO maxTimeSlice input


-- Example 2: A producer and consumer, linked by a buffer.
--
example2 :: Int -> IO I.Value
example2 maxTimeSlice =
  let
    input =
      "let                                    \
      \  buffer = 0                           \
      \in                                     \
      \let                                    \
      \  producer =                           \
      \    proc (n)                           \
      \      letrec                           \
      \        wait(k) =                      \
      \          if zero?(k) then             \
      \            set buffer = n             \
      \          else                         \
      \            begin                      \
      \              print(-(k, -(0, 200)));  \
      \              (wait -(k, 1))           \
      \            end                        \
      \      in                               \
      \      (wait 5)                         \
      \in                                     \
      \let                                    \
      \  consumer =                           \
      \    proc (d)                           \
      \      letrec                           \
      \        busywait(k) =                  \
      \          if zero?(buffer) then        \
      \            begin                      \
      \              print(-(k, -(0, 100)));  \
      \              (busywait -(k, -(0, 1))) \
      \            end                        \
      \          else                         \
      \            buffer                     \
      \      in                               \
      \      (busywait 0)                     \
      \in                                     \
      \begin                                  \
      \  spawn(proc (d) (producer 44));       \
      \  print(300);                          \
      \  (consumer 86)                        \
      \end                                    "
  in
  I.runIO maxTimeSlice input
--
-- Results from a test run (maxTimeSlice = 43):
--
-- 300
-- 100
-- 101
-- 205
-- 204
-- 203
-- 102
-- 103
-- 104
-- 202
-- 201
-- "End of subthread computation"
-- 105
-- "End of main thread computation"
-- 44
--


-- Example 3a: An unsafe counter.
--
example3a :: Int -> IO I.Value
example3a maxTimeSlice =
  let
    input =
      "let                              \
      \  x = 0                          \
      \in                               \
      \let                              \
      \  incrx =                        \
      \    proc (id)                    \
      \      proc (dummy)               \
      \        begin                    \
      \          set x = -(x, -(0, 1)); \
      \          print(-(id, -(0, x)))  \
      \        end                      \
      \in                               \
      \  begin                          \
      \    spawn((incrx 100));          \
      \    spawn((incrx 200));          \
      \    spawn((incrx 300))           \
      \  end                            "
  in
  I.runIO maxTimeSlice input
-- Results:
--
-- All executions give: 101, 202, 303.
--
-- NOTE: The problem doesn't show itself because of how the scheduler is
-- implemented.
--
-- 1. Each thread gets an equal time slice.
-- 2. Threads are executed in-turn.
--
-- Let's try something else then:


example3b :: Int -> IO I.Value
example3b maxTimeSlice =
  let
    input =
      "let                              \
      \  x = 0                          \
      \in                               \
      \let                              \
      \  incra =                        \
      \    proc (id)                    \
      \      proc (dummy)               \
      \        begin                    \
      \          id; id;                \
      \          set x = -(x, -(0, 1)); \
      \          print(-(id, -(0, x)))  \
      \        end                      \
      \in                               \
      \let                              \
      \  incrb =                        \
      \    proc (id)                    \
      \      proc (dummy)               \
      \        begin                    \
      \          id;                    \
      \          set x = -(x, -(0, 1)); \
      \          print(-(id, -(0, x)))  \
      \        end                      \
      \in                               \
      \let                              \
      \  incrc =                        \
      \    proc (id)                    \
      \      proc (dummy)               \
      \        begin                    \
      \          set x = -(x, -(0, 1)); \
      \          print(-(id, -(0, x)))  \
      \        end                      \
      \in                               \
      \  begin                          \
      \    spawn((incra 100));          \
      \    spawn((incrb 200));          \
      \    spawn((incrc 300))           \
      \  end                            "
  in
  I.runIO maxTimeSlice input
-- Results:
--
-- 1 - 101, 201, 302
-- 4 - 101, 202, 303
-- 5 - 101, 202, 302
-- 6 - 201, 101, 302
-- 8 - 101, 203, 303
-- 100 - 101, 202, 303
--
-- Can I get it to produce 1?
--
-- i.e. the sequence: 101, 201, 301.


-- Example 4: A safe counter using a mutex.
--
example4 :: Int -> IO I.Value
example4 maxTimeSlice =
  let
    input =
      "let                              \
      \  x = 0                          \
      \in                               \
      \let                              \
      \  mut = mutex()                  \
      \in                               \
      \let                              \
      \  incrx =                        \
      \    proc (id)                    \
      \      proc (dummy)               \
      \        begin                    \
      \          wait(mut);             \
      \          set x = -(x, -(0, 1)); \
      \          print(-(id, -(0, x))); \
      \          signal(mut)            \
      \        end                      \
      \in                               \
      \  begin                          \
      \    spawn((incrx 100));          \
      \    spawn((incrx 200));          \
      \    spawn((incrx 300))           \
      \  end                            "
  in
  I.runIO maxTimeSlice input


-- Example 5: An example for Exercise 5.45.
example5 :: Int -> IO I.Value
example5 maxTimeSlice =
  let
    input =
      "let                                      \
      \  printloop =                            \
      \    proc (id)                            \
      \      proc (n)                           \
      \        letrec                           \
      \          loop (n) =                     \
      \            if zero?(-(n, 20)) then      \
      \              print(yield)               \
      \            else                         \
      \              if zero?(n) then           \
      \                print(id)                \
      \              else                       \
      \                begin                    \
      \                  print(-(id, -(0, n))); \
      \                  (loop -(n, 1))         \
      \                end                      \
      \        in                               \
      \        (loop n)                         \
      \in                                       \
      \begin                                    \
      \  spawn(proc (d) ((printloop 100) 25));  \
      \  spawn(proc (d) ((printloop 200) 10));  \
      \  ((printloop 300) 5)                    \
      \end                                      "
  in
  I.runIO maxTimeSlice input
