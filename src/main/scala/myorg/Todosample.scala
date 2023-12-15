package myorg

import cats.effect.IO
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js.annotation.*

@JSExportTopLevel("TyrianApp")
object Todosample extends TyrianApp[Msg, Model]:
  def router: Location => Msg = Routing.none(Msg.NoOp)

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) = 
    (Todo.init, Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case Msg.Update(s) => (Todo(model._1, s), Cmd.None)
    case Msg.Create => (Todo(Item.init(model.nextTodo) :: model.items, ""), Cmd.None)
    case Msg.Modify(id, m) =>
      Item.update(m, model.items(id)) match
        case Some(item) => 
          (Todo(model.items.updated(id, item), model.nextTodo), Cmd.None)
        case None => 
          (Todo(model.items.patch(id, Nil, 1), model.nextTodo), Cmd.None)
    case Msg.NoOp => (model, Cmd.None)

  def view(model: Model): Html[Msg] =
    val todos = model.items.zipWithIndex.map { case (c, i) =>
      Item.view(c).map(msg => Msg.Modify(i, msg))
    }

    val elems = div(cls := "input-wrap")(
      input(onInput(txt => Msg.Update(txt)), value := model.nextTodo),
      button(onClick(Msg.Create))(text("Add"))
    ) :: todos

    div(cls := "todo-app")(h1(cls := "app-title")("Todo List") :: elems)

  def subscriptions(model: Model): Sub[IO, Msg] = Sub.None

case class Todo(items: List[Item.Model], nextTodo: String)
object Todo:
  def init = Todo(Nil, "")
type Model = Todo

enum Msg:
  case Update(s: String)
  case Create
  case Modify(i: Int, msg: Item.Msg)
  case NoOp

object Item:
  case class TodoItem(todo: String, status: Status)
  opaque type Model = TodoItem

  enum Status:
    case Done
    case Do

  extension(s: Status)
    def next = s match
      case Status.Done => Status.Do
      case Status.Do => Status.Done
    def str = s match
      case Status.Done => "done" 
      case Status.Do => "do"

  def init(todo: String): Model = TodoItem(todo, Status.Do)

  enum Msg:
    case Toggle
    case Remove

  def view(model: Model): Html[Msg] =
    div(cls := s"${model.status.str} todo-item")(
      p(cls := "todo-title")(model.todo),
      button(onClick(Msg.Toggle))(model.status.next.str),
      button(cls := "red", onClick(Msg.Remove))("Remove")
    )

  def update(msg: Msg, model: Model): Option[Model] =
    msg match
      case Msg.Toggle => Some(model.copy(status = model.status.next))
      case Msg.Remove => None 
