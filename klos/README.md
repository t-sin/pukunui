# KLOS - Kiwi Lightweight Object System

## Motivation

On writing [Pukunui](https://github.com/t-sin/pukunui) I decided not to use CLOS. There are some reasons of it, mainly because of unnecessarity of some CLOS features; dynamic class redefinition, method combinations.

## Goals

The KLOS (Kiwi Lightweight Object System) will be raise to define class things to represent *unit generators* in Pukunui. KLOS will be designed to aim some features below:

- persistent class definition
    - cannot modify class definition after the class has be defined
- fast method dispatching
- simple type hierarchy
    - and there is an another root object

## Specs

- KLOS stores class definition in an array (`*class-table*`)
    - for performance; old object system in Pukunui probably has an issue about method dispatching
- L1: class definition API
    - `make-class`
    - `find-class`: name -> class definition
- L2: instances API
    - `instantiate`: name :keyword params -> object
    - `class-of`: object -> class definition
    - `slot-value`: object slot-name -> value
- L3: method API
    - `make-generic-method`
    - `dispatch-method`
- L4: macros
    - `defclass`
    - `with-slots`
