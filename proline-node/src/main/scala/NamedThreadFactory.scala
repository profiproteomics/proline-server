package fr.proline.jms

import java.util.concurrent.ThreadFactory


class NamedThreadFactory(name: String) extends  ThreadFactory {

  override def newThread(r: Runnable):Thread  = {
    return new Thread(name)
  }
}
