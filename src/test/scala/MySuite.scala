class MySuite extends munit.FunSuite {
  // Example test that succeeds
  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }
  //**TODO: Your tests here
  // simple tests for ValueExpr()
  test("eval(ValueExpr(Cry)) should be Cry") {
    val obtained = eval(ValueExpr(Cry))
    val expected = Right(Cry)
    assertEquals(obtained, expected)
  }

  test("eval(ValueExpr(Happy)) should be Happy") {
    val obtained = eval(ValueExpr(Happy))
    val expected = Right(Happy)
    assertEquals(obtained, expected)
  }

  test("eval(ValueExpr(Sleepy)) should be Sleepy") {
    val obtained = eval(ValueExpr(Sleepy))
    val expected = Right(Sleepy)
    assertEquals(obtained, expected)
  }

  test("eval(ValueExpr(Stun)) should be Stun") {
    val obtained = eval(ValueExpr(Stun))
    val expected = Right(Stun)
    assertEquals(obtained, expected)
  }

  // test for ManyExprs() list
  test("eval(ManyExprs(List(ValueExpr(Cry), ValueExpr(Stun), ValueExpr(Sleepy), ValueExpr(Happy)))) should be ManyVals(List(Cry, Stun, Sleepy, Happy))") {
    val expr = ManyExprs(List(ValueExpr(Cry), ValueExpr(Stun), ValueExpr(Sleepy), ValueExpr(Happy)))
    val obtained = eval(expr)
    val expected = Right(ManyVals(List(Cry, Stun, Sleepy, Happy)))
    assertEquals(obtained, expected)
  }

  // test specific plus logic
  test("eval(Plus(ValueExpr(Cry), ValueExpr(Happy))) should be Happy") {
    val expr = Plus(ValueExpr(Cry), ValueExpr(Happy))
    val obtained = eval(expr)
    val expected = Right(Happy)
    assertEquals(obtained, expected)
  }

// Test specific Plus logic with ManyExprs
  test("eval(Plus(ManyExprs(List(ValueExpr(Cry), ValueExpr(Stun))), ValueExpr(Happy))) should handle ManyExprs correctly") {
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


  test("Not(Sleepy) -> Stun") {
    val expr = Not(ValueExpr(Sleepy))
    val obtained = eval(expr)
    val expected = Right(Stun)
    assertEquals(obtained, expected)
  }

  // Test specific Not logic with ManyExprs
  test("Not(Stun, Sleepy, Happy, Cry) -> Happy") {
    val expr = Not(
      ManyExprs(List(
        ValueExpr(Stun),
        ValueExpr(Sleepy),
        ValueExpr(Happy),
        ValueExpr(Cry)
      ))
    )

    // Evaluate ManyExprs
    // ManyExprs(List(ValueExpr(Stun), ValueExpr(Sleepy), ValueExpr(Happy), ValueExpr(Cry)))
    // -> Happy

    val obtained = eval(expr)
    val expected = Right(Happy)

    assertEquals(obtained, expected)
  }


  // Complex test 1
  test("Complex test 1") {
    val expr = 
      Plus(
        ManyExprs(List(ValueExpr(Cry), ValueExpr(Stun), ValueExpr(VeryHappy))),
        Not(ValueExpr(Sleepy))
      )
  
    // Evaluate ManyExprs
    // ManyExprs(List(ValueExpr(Cry), ValueExpr(Stun), ValueExpr(VeryHappy))) 
    // -> ManyVals(List(Cry, Stun, VeryHappy))

    // Evaluate Not(ValueExpr(Sleepy)) -> Stun

    // Evaluate Plus(ManyVals(List(Cry, Stun, VeryHappy)), Stun)
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
  
    // Evaluate the first ManyExprs
    // ManyExprs(List(ValueExpr(Cry), ValueExpr(Sleepy), ValueExpr(Happy), ValueExpr(Stun)))
    // This evaluates to: ManyVals(List(Cry, Sleepy, Happy, Stun))
  
    // Evaluate Not on ManyVals
    // Not(ManyVals(List(Cry, Sleepy, Happy, Stun))) -> Happy


    // Evaluate the second ManyExprs
    // ManyExprs(List(ValueExpr(VeryHappy), ValueExpr(Cry), ValueExpr(Sleepy)))
    // This evaluates to: ManyVals(List(VeryHappy, Cry, Sleepy))

    // Combine results with Plus
    // Plus(Happy, ManyVals(List(VeryHappy, Cry, Sleepy)))
    // Evaluate Plus for each combination of values:
    // Plus(Happy, VeryHappy) -> VeryHappy
    // Plus(Happy, Cry) -> Happy
    // Plus(Happy, Sleepy) -> Happy

    val obtained = eval(expr)

    // Expected result
    val expected = Right(ManyVals(List(VeryHappy, Happy, Happy)))
  
    // Assert the obtained result matches the expected result
    assertEquals(obtained, expected)
  }


}
