package pathway

import org.scalatest.FunSuite
import pathway._

class gseaTest extends FunSuite {
  var expFile = """./resources/LUAD.rnaseqv2__illuminahiseq_rnaseqv2__unc_edu__Level_3__RSEM_genes_normalized__data.data.txt"""
  val geneSetFile = "./resources/c2.cp.kegg.v5.2.entrez.set.txt"
  var targetGene = "5976"

  test("gsea"){
    pathway.Gsea.outFile = "./results/GseaResultTest0.txt"
    pathway.Gsea.gseaFile = "./resources/GseaoutTest0.txt"
    val gseax = pathway.Gsea.getGsea(targetGene,expFile,geneSetFile,100,perm = true)
    assert(gseax(0).setName == "KEGG_OXIDATIVE_PHOSPHORYLATION")
    assert(gseax(0).esScore == -0.63846517f)
  }
  test("gsea without limma"){
    pathway.Gsea.RoutputFile = "./resources/LimmaPermResultsTest.txt"
    pathway.Gsea.outFile = "./results/GseaResultTest.txt"
    pathway.Gsea.gseaFile = "./resources/GseaoutTest.txt"
    val gseax = pathway.Gsea.getGsea(targetGene,expFile,geneSetFile,100,perm = false)
    assert(gseax(0).setName == "KEGG_OXIDATIVE_PHOSPHORYLATION")
    assert(gseax(0).esScore == -0.63846517f)
  }

}