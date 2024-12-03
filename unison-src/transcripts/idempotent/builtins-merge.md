The `builtins.merge` command adds the known builtins to the specified subnamespace within the current namespace.

``` ucm
scratch/main> builtins.merge builtins

  Done.

scratch/main> ls builtins

  1.  Any                 (builtin type)
  2.  Any/                (2 terms)
  3.  Boolean             (builtin type)
  4.  Boolean/            (1 term)
  5.  Bytes               (builtin type)
  6.  Bytes/              (34 terms)
  7.  Char                (builtin type)
  8.  Char/               (22 terms, 1 type)
  9.  ClientSockAddr      (builtin type)
  10. Code                (builtin type)
  11. Code/               (9 terms)
  12. Debug/              (3 terms)
  13. Doc                 (type)
  14. Doc/                (6 terms)
  15. Either              (type)
  16. Either/             (2 terms)
  17. Exception           (type)
  18. Exception/          (1 term)
  19. Float               (builtin type)
  20. Float/              (38 terms)
  21. Handle/             (1 term)
  22. ImmutableArray      (builtin type)
  23. ImmutableArray/     (3 terms)
  24. ImmutableByteArray  (builtin type)
  25. ImmutableByteArray/ (8 terms)
  26. Int                 (builtin type)
  27. Int/                (31 terms)
  28. IsPropagated        (type)
  29. IsPropagated/       (1 term)
  30. IsTest              (type)
  31. IsTest/             (1 term)
  32. Link                (type)
  33. Link/               (3 terms, 2 types)
  34. List                (builtin type)
  35. List/               (10 terms)
  36. ListenSocket        (builtin type)
  37. MutableArray        (builtin type)
  38. MutableArray/       (6 terms)
  39. MutableByteArray    (builtin type)
  40. MutableByteArray/   (14 terms)
  41. Nat                 (builtin type)
  42. Nat/                (28 terms)
  43. Optional            (type)
  44. Optional/           (2 terms)
  45. Pattern             (builtin type)
  46. Pattern/            (9 terms)
  47. Ref                 (builtin type)
  48. Ref/                (2 terms)
  49. Request             (builtin type)
  50. RewriteCase         (type)
  51. RewriteCase/        (1 term)
  52. RewriteSignature    (type)
  53. RewriteSignature/   (1 term)
  54. RewriteTerm         (type)
  55. RewriteTerm/        (1 term)
  56. Rewrites            (type)
  57. Rewrites/           (1 term)
  58. Scope               (builtin type)
  59. Scope/              (6 terms)
  60. SeqView             (type)
  61. SeqView/            (2 terms)
  62. Socket/             (1 term)
  63. Test/               (2 terms, 1 type)
  64. Text                (builtin type)
  65. Text/               (34 terms)
  66. ThreadId/           (1 term)
  67. Tuple               (type)
  68. Tuple/              (1 term)
  69. UDPSocket           (builtin type)
  70. Unit                (type)
  71. Unit/               (1 term)
  72. Universal/          (7 terms)
  73. Value               (builtin type)
  74. Value/              (5 terms)
  75. bug                 (a -> b)
  76. crypto/             (17 terms, 2 types)
  77. io2/                (146 terms, 32 types)
  78. metadata/           (2 terms)
  79. todo                (a -> b)
  80. unsafe/             (1 term)
```