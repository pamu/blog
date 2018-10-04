---
    title: Queue in functional programming languages
    author: Nagarjuna Pamu
---

```haskell
data List a = Nil | Cons a (List a) deriving (Show)
```

We all know that cons list is the very popular in functional programming data
structures. The time complexity of the cons list for prepending an element to the
existing list is constant, but appending takes linear time complexity and also linear
space complexity.


So, One data structure that can be modeled naturally with the help of cons list
is `Stack`. Prepending operation can be used for pushing an element on top of the stack.
In order to pop the element, all that is needed is to drop the first element of the
cons list and retain the tail.

Good thing is prepending and dropping the first element happens in const space and time complexity.


___Notice that appending time complexity and space complexity is linear for cons list.___

This Results in lot of garbage when appending operation happens on the cons list.
Usually algorithms which use immutable data structures use prepending operation and
reverse the list as the final operation before returning the final result (list).


Lets now see How we can model a `Queue` in a immutable fashion using basic building
in two functional languages. First in Haskell and in Scala.

### Code in Haskell

Immutable queue

```haskell
data Queue a =
  Queue [a] -- Dequeue list
        [a] -- Enqueue list
  deriving (Show)
```

Dequeue list is for

Building a immutable empty Queue

```haskell
empty :: Queue a
empty = Queue [] []
```

Enqueue operation. Prepending the incoming element to the head of the enqueue list.
Note this is a constant time and space operation.

```haskell
enqueue :: a -> Queue a -> Queue a
enqueue elem (Queue xs ys) = Queue xs (elem : ys)
```

Dequeue mantra

  - When both dequeue list (first list) and enqueue list (second list) is empty.
    Then that means queue is empty

  - When dequeue list is not empty (first list) then remove element from the head
    of the dequeue list and then update the dequeue part of the queue with the
    tail of the dequeue list (first list)

  - When Dequeue list is empty. Load values from the enqueue list (second list).
    While loading remember to reverse the enqueue list to maintain the order of
    removal (dequeuing) of the elements from the queue.

```haskell
tryDequeue :: Queue a -> Maybe (a, Queue a)
tryDequeue (Queue [] []) = Nothing
tryDequeue (Queue (x:xs) ys) = Just (x, Queue xs ys)
tryDequeue (Queue [] ys) = Just (head, Queue tail [])
  where
    (head:tail) = reverse ys
```


### Code in Scala

Scala code. Check haskell explaination.

```scala
case class Queue[A](dequeueList: List[A], enqueueList: List[A]) {

  def enqueue(elem: A): Queue[A] = this match {
    case Queue(xs, ys) => Queue(xs, elem :: ys)
  }

  def tryDequeue: Option[(A, Queue[A])] = this match {
    case Queue(Nil, Nil) => None
    case Queue(x :: xs, ys) => Some(x -> Queue(xs, ys))
    case Queue(Nil, ys) =>
      val head :: tail = ys.reverse
      Some(head -> Queue(tail, Nil))
  }
}

object Queue {
  def empty[A]: Queue[A] = Queue(Nil, Nil)
}
```

### Live coding videos

#### Haskell live coding video

<iframe width="560" height="315" src="https://www.youtube.com/embed/bJC6ajEsSVI" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>


#### Scala live coding video

<iframe width="560" height="315" src="https://www.youtube.com/embed/mEQzlj_RVJ8" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>
