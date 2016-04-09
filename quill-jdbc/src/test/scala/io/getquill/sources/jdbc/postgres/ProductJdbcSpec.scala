package io.getquill.sources.jdbc.postgres

import io.getquill._
import io.getquill.sources.sql.ProductSpec

class ProductJdbcSpec extends ProductSpec {

  override def beforeAll = {
    testPostgresDB.run(quote(query[Product].delete))
    ()
  }

  "Product" - {
    //    "Insert multiple products" in {
    //      val inserted = testPostgresDB.run(productInsert)(productEntries)
    //      val product = testPostgresDB.run(productById)(inserted(2)).head
    //      product.description mustEqual productEntries(2).description
    //      product.id mustEqual inserted(2)
    //    }
    //
    //    "Single insert product" in {
    //      val inserted = testPostgresDB.run(productSingleInsert)
    //      val product = testPostgresDB.run(productById)(inserted).head
    //      product.description mustEqual "Window"
    //      product.id mustEqual inserted
    //    }

    "Single product with Free Variable" in {
      val prd = Product(0L, "test123", 428112L)
      val q1 = quote { product.insert(_.sku -> prd.sku, _.description -> prd.description) }

      val inserted = testPostgresDB.run(q1)
      val returnedProduct = testPostgresDB.run(productById)(inserted).head
      returnedProduct.description mustEqual "test123"
      returnedProduct.sku mustEqual 428112L
      returnedProduct.id mustEqual inserted

    }
  }

}
