class MySuite extends munit.FunSuite {
  // Example test that succeeds
  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }
  //**TODO: Your tests here
  // simple tests for ValueExpr()
  test("ValueExpr(Cry) -> Cry") {
    val obtained = eval(ValueExpr(Cry))
    val expected = Right(Cry)
    assertEquals(obtained, expected)
  }

  test("ValueExpr(Happy) -> Happy") {
    val obtained = eval(ValueExpr(Happy))
    val expected = Right(Happy)
    assertEquals(obtained, expected)
  }

  test("ValueExpr(Sleepy) -> Sleepy") {
    val obtained = eval(ValueExpr(Sleepy))
    val expected = Right(Sleepy)
    assertEquals(obtained, expected)
  }

  test("ValueExpr(Stun) -> Stun") {
    val obtained = eval(ValueExpr(Stun))
    val expected = Right(Stun)
    assertEquals(obtained, expected)
  }

  // test for ManyExprs() list
  test("ManyExprs(List(ValueExpr(Cry), ValueExpr(Stun), ValueExpr(Sleepy), ValueExpr(Happy))) -> ManyVals(List(Cry, Stun, Sleepy, Happy))") {
    val expr = ManyExprs(List(ValueExpr(Cry), ValueExpr(Stun), ValueExpr(Sleepy), ValueExpr(Happy)))
    val obtained = eval(expr)
    val expected = Right(ManyVals(List(Cry, Stun, Sleepy, Happy)))
    assertEquals(obtained, expected)
  }

  // test specific plus logic
  test("Plus(ValueExpr(Cry), ValueExpr(Happy))) -> Happy") {
    val expr = Plus(ValueExpr(Cry), ValueExpr(Happy))
    val obtained = eval(expr)
    val expected = Right(Happy)
    assertEquals(obtained, expected)
  }

// Test specific Plus logic with ManyExprs
  test("Plus(ManyExprs(List(ValueExpr(Cry), ValueExpr(Stun))), ValueExpr(Happy))) -> Happy,Happy") {
    val expr = Plus(
    ManyExprs(List(ValueExpr(Cry), ValueExpr(Stun))),  // Evaluate to ManyVals(List(Cry, Stun))
    ValueExpr(Happy)  // Evaluate to Happy
  )

    // Expected result
    // Combine ManyVals(List(Cry, Stun)) with Happy
    // Plus(Cry, Happy) -> Happy
    // Plus(Stun, Happy) -> Happy
    // This evaluates to ManyVals(List(Happy, Happy))
    val obtained = eval(expr)
    val expected = Right(ManyVals(List(Happy, Happy)))
  
    assertEquals(obtained, expected)
}

  // specific Not logic
  test("Not(Sleepy) -> Stun") {
    val expr = Not(ValueExpr(Sleepy))
    val obtained = eval(expr)
    val expected = Right(Stun)
    assertEquals(obtained, expected)
  }

  // Test specific Not logic with ManyExprs
  test("Not(Stun, Sleepy, Happy, Cry) -> Cry") {
    // Plus(Stun,Sleepy) = Stun
    // Plus(Stun,Happy) -> Happy
    // Plus(happy,cry) -> Happy
    // Not(Happy) -> Cry
    val expr = Not(
      ManyExprs(List(
        ValueExpr(Stun),
        ValueExpr(Sleepy),
        ValueExpr(Happy),
        ValueExpr(Cry)
      ))
    )
    val obtained = eval(expr)
    val expected = Right(Cry)

    assertEquals(obtained, expected)
  }


  // Complex test 1
  test("Complex test 1") {
    val expr = 
      Plus(
        ManyExprs(List(ValueExpr(Cry), ValueExpr(Stun), ValueExpr(VeryHappy))),
        Not(ValueExpr(Sleepy))
      )
  
    // ManyExprs(List(ValueExpr(Cry), ValueExpr(Stun), ValueExpr(VeryHappy))) 
    // -> ManyVals(List(Cry, Stun, VeryHappy))

    // Not(ValueExpr(Sleepy)) -> Stun
    // Plus(ManyVals(List(Cry, Stun, VeryHappy)), Stun)
    // Plus(Cry, Stun) -> Stun
    // Plus(Stun, Stun) -> Cry
    // Plus(VeryHappy, Stun) -> VeryHappy
    // Result should be ManyVals(List(Stun, Cry, VeryHappy))

    val obtained = eval(expr)
    val expected = Right(ManyVals(List(Stun, Cry, VeryHappy)))  
    assertEquals(obtained, expected)
  }

  // Complex test 2
  test("Complex test 2") {
    val expr = Plus(
      Not(
        ManyExprs(List(
          ValueExpr(Cry),
          ValueExpr(Sleepy),
          ValueExpr(Happy),
          ValueExpr(Stun)
        ))
      ),
      ManyExprs(List(
        ValueExpr(VeryHappy),
        ValueExpr(Cry),
        ValueExpr(Sleepy)
      ))
    )
  
    // ManyExprs(List(ValueExpr(Cry), ValueExpr(Sleepy), ValueExpr(Happy), ValueExpr(Stun)))
    // -> ManyVals(List(Cry, Sleepy, Happy, Stun))
  
    // Evaluate Not on ManyVals
    // Not(ManyVals(List(Cry, Sleepy, Happy, Stun))) -> Cry

    // ManyExprs(List(ValueExpr(VeryHappy), ValueExpr(Cry), ValueExpr(Sleepy)))
    // -> ManyVals(List(VeryHappy, Cry, Sleepy))

    // Combine results with Plus
    // Plus(Happy, ManyVals(List(VeryHappy, Cry, Sleepy)))
    // Evaluate Plus for each combination of values:
    // Plus(Cry, VeryHappy) -> VeryHappy
    // Plus(Cry, Cry) -> Cry
    // Plus(Cry, Sleepy) -> Sleepy

    val obtained = eval(expr)
    val expected = Right(ManyVals(List(VeryHappy, Cry, Sleepy)))
  
    assertEquals(obtained, expected)
  }


}
