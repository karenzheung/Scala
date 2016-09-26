package git

import java.io.File

import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.api.Git
import org.scalatest.FunSuite

/**
 * This suite checks proper git usage.
 */
class GitSuite extends FunSuite {

  /**
   * Returns the student's git repository.
   *
   * @return the repository
   */
  def repository: Repository = {
    try {
      (new FileRepositoryBuilder)
        .setGitDir(new File(".git"))
        .readEnvironment()
        .findGitDir()
        .build()
    }
    catch {
      case e: Throwable => fail("The submission does not have a proper git repository")
    }
  }

  /**
   * This test ensures that the student's submission is a git repository.
   */
  test("[5!] A submission must have a git repository") {
    val logs = new Git(repository).log().call().iterator()
    // If we reach this point then we have a proper git repository
    assert(true)
  }

  test("[5] A submission must include 5 commits") {
    // Get the log entries.
    val logs = new Git(repository).log().call().iterator()

    // We need to use Java's iterator since jgit is implemented in Java.
    var count = 0
    while (logs.hasNext) {
      count += 1
      logs.next()
    }

    // Check the count
    assert(count >= 5, s"The submission had $count/5 commits")
  }

}
