package io.getquill.norm.capture

import io.getquill._
import io.getquill.Spec
import language.reflectiveCalls

class AvoidAliasConflictSpec extends Spec {

  "renames alias to avoid conflict between queryables during normalization" in {
    val q = quote {
      qr1.filter(u => u.s == "s1").flatMap(u => qr2.flatMap(u => qr3.filter(u => u.s == "s1").map(u => u.s)))
    }
    val n = quote {
      qr1.filter(u => u.s == "s1").flatMap(u => qr2.flatMap(u1 => qr3.filter(u2 => u2.s == "s1").map(u => u.s)))
    }
    AvoidAliasConflict(q.ast) mustEqual n.ast
  }

  "doesn't change the query if it doesn't have conflicts" in {
    val q = quote {
      qr1.flatMap(a => qr2.filter(b => b.s == "s1")).flatMap(c => qr3.map(d => d.s))
    }
    AvoidAliasConflict(q.ast) mustEqual q.ast
  }
}