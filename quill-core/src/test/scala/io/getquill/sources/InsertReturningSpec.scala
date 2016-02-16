package io.getquill.sources

import io.getquill._
import io.getquill.sources.mirror.Row
import io.getquill.TestSource.mirrorSource

class InsertReturningSpec extends Spec {
  "runs insert returning action" - {
    "runs batched action" - {
      "one param" in {
        val q = quote {
          (p1: Int) => qr1.insert(_.i -> p1)
        }
        val r = mirrorSource.run(q)(List(1, 2))
        r.ast mustEqual q.ast.body
        r.bindList mustEqual List(Row(1), Row(2))
      }
      "two params" in {
        val q = quote {
          (p1: Int, p2: String) => qr1.insert(_.i -> p1, _.s -> p2)
        }
        val r = mirrorSource.run(q)(List((1, "a"), (2, "b")))
        r.ast mustEqual q.ast.body
        r.bindList mustEqual List(Row(1, "a"), Row(2, "b"))
      }
    }
    "one return" in {
      val q = quote {
        qr1.returning(b => b.i).insert
      }

      val r = mirrorSource.run(q)(List((1, "a"), (2, "b")))
    }
  }
}