# Architecture

This file is intended as a reasonable of reflection of the high-level
code layout. It also contains some musings on the design space of
optimization solving. The current aim is only to deal with continuous
optimization but to otherwise leave the input data unconstrained and
also allowing for the presence of noise in the data.

## Core concepts

As much as possible all concepts should be captured with datatypes and
only interpreted into code later. The fundamental topics are that of a
problem to be optimized and a of strategies used to

### The optimzation problem

In general an optimization problem is specified by the following data.

* The objective function,
* the equality constraints,
* the inequality constraints.

### The strategy

The strategy is essentialy an arbitrary algorithm that is allowed to
inspect the problem (e.g. to sample the objective function at various
points) and probabilistically update its internal state.

The strategies usually come with lots of hyperparameters that also
need to be optimized for the method to be effective. It would be
great if we could use other (parameterless) strategies to do this too.

### The driver

The driver is used to call some strategy repeatedly on the given
problem until the desired accuracy, say, is reached.

## Modeling

Currently the problem is a record with a probabilistic objective
function and a random region expressing an inequality constraint (is
this general enough to allow for an unconstrained problems?). It seems
likely that this will have to be relaxed to allow for various classes
of problems. That will in turn affect the design of the Strategy type
class.

# Design space

There are many issues when it comes to solving optimization
problems. They seem to fall broadly into two categories: which
problems do we tackle and what kind of interface do we expose.

## The class of problems

There are at least the following important classes of problems.

* deterministic or stochastic,
* convex or general,
* differentiable or general.

In all of these the class on the left has more structure and thus
should be easier to exploit. Ideally, we'd like the user express the
kind of structure their problem has and use the best optimization
strategy available. But there is no reason why a more general strategy
couldn't be applied to a more structured problem.

In contrast the following classes are exclusive.

* continuous or discrete,
* constrained or unconstrained,
* fixed input data or black box sampling.

For example, a strategy that treats a continuous problem is not
expected to know what to do with a discrete variant and vice
versa. There are ways of transforming some of these classes into the
other, and these might eventually supported. However, currently the
main focus of this library is to only treat *unconstrained*
*continuous* *sampling* problems.

## The interface

The users of this library should be given maximum control both when
specifying their problem and the strategy hyperparameters and when
driving the strategy towards the solution.

Since the problems are stochastic in general, it seems like a
streaming API (such as Pipes) would be a good fit. Thus a strategy,
once initialized with a problem and a set of hyperparameters, should be
a producer of states converging to optimal values.

(A note to self on terminology: strategy/method/solver/stepper are essentially
just different facets of the same thing. Pick the best names for the
types and functions.)