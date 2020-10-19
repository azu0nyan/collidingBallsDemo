/* Максимально простой способ запустить нестатичную графику и управление с клавиатуры в своей программе.
 * В угоду простоте ниже используются нелучшие практики программирования
 */
package main

//првоерьте что у вас правильно написан пакет(это должен быть путь к этому файлу от корня папки с кодом


import java.awt._
import java.awt.event.{KeyEvent, KeyListener}

import javax.swing._

import scala.util.Random


/**
 * Класс - окно выводящаеся на экран благодаря extends JFrame
 * И обрабатывающее нажатия клавиш благодаря implements KeyListener
 * Название класса должно совпадать с именем файла(как и всегда)
 */
object Graphics { //Тут храним всякие данные


  val balls: Seq[Circle] = Seq(
    new Circle(4000, V2(500, -3950), restitution = 1, dynamic = false),
    new Circle(3000, V2(500, 768 + 2850), restitution = 1, dynamic = false),
    new Circle(1500, V2(-1300, 500), restitution = 1, dynamic = false),
    new Circle(1000, V2(2000, 500), restitution = 1, dynamic = false),

  ) ++ (for (i <- 0 until 30) yield
    new Circle(10, V2(new Random().nextInt(1000) + 100,
      new Random().nextInt(500) + 100)))


  def draw(g: Graphics2D): Unit = {
    balls.foreach { b =>
      b.updateMe(1 / 60.0)
    }
    balls.foreach(b => b.drawMe(g))

  }

  val w = 1920
  val h = 1080
  //магический код позволяющий всему работать, лучше не трогать
  @throws[InterruptedException]
  def main(args: Array[String]): Unit = {
    System.setProperty("sun.java2d.opengl", "true")
    val jf = new JFrame()
    jf.setSize(w, h) //размер экрана

    jf.setUndecorated(false) //показать заголовок окна

    jf.setTitle("Моя супер программа")
    jf.setVisible(true)
    //    jf.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    jf.createBufferStrategy(2)
    jf.addKeyListener(KeyListener)
    //в бесконечном цикле рисуем новый кадр
    while ( {
      true
    }) {
      val frameLength = 1000 / 60 //пытаемся работать из рассчета  60 кадров в секунду
      val start = System.currentTimeMillis
      val bs = jf.getBufferStrategy
      val g = bs.getDrawGraphics.asInstanceOf[Graphics2D]
      g.clearRect(0, 0, jf.getWidth, jf.getHeight)
      draw(g)
      bs.show()
      g.dispose()
      val `end` = System.currentTimeMillis
      val len = `end` - start
      if (len < frameLength) Thread.sleep(frameLength - len)
    }
  }
}

object KeyListener extends KeyListener {
  override def keyPressed(e: KeyEvent): Unit = { //Вариант 1 (управляем красным шариком)
    e.getKeyCode match {
      case KeyEvent.VK_W =>
        Graphics.balls(0).acc = V2(0, -400)
      case _ =>

    }
  }
  override def keyTyped(e: KeyEvent): Unit = {
  }
  //Вызывается когда клавиша отпущена пользователем, обработка события аналогична keyPressed
  override def keyReleased(e: KeyEvent): Unit = {
    e.getKeyCode match {
      case KeyEvent.VK_W =>
        Graphics.balls(0).acc = V2(0, 0)
      case _ =>

    }
  }
}
