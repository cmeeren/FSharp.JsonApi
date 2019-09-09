module Db

// This module is just a mock data store and functions to query and modify it
// (pretending they're async to make it realistic). It has nothing directly to
// do with FSharp.JsonApi.


open Domain


// Pre-defined data

let private ada = Person.create "Ada" "Lovelace" (Some None) (Some (Some Female))
let private eddie = Person.create "Eddie" "Izzard" (Some (Some "@eddieizzard")) None
let private tom = Person.create "Tom" "Hanks" (Some (Some "@tomhanks")) (Some (Some Male))
let private article = Article.create tom.Id "Life is like a box of chocolates" "<p>My mom always said life was like a box of chocolates. You never know what you're gonna get. Well, as long as it's Lindt Lindor Milk Chocolate Truffles, I could eat about a million and a half!</p>" (Some Commercial)
let private comment1 = Comment.create eddie.Id article.Id "First! Also, being unafraid to dress in a dress makes me twice as awesome."
let private comment2 = Comment.create ada.Id article.Id "u there babbage?"


// Data storage

let mutable private persons =
  [ada; eddie; tom]
  |> List.map (fun p -> p.Id, p)
  |> Map.ofList

let mutable private articles =
  [article]
  |> List.map (fun a -> a.Id, a)
  |> Map.ofList

let mutable private comments =
  [comment1; comment2]
  |> List.map (fun c -> c.Id, c)
  |> Map.ofList



let safeSkip count xs =
  if xs |> List.length < count
  then xs
  else xs |> List.skip count


// Functions to query/modify data

module Person =

  let search (searchArgs: PersonSearchArgs) =
    async {
      let sortF f xs =
        if searchArgs.SortDescending then List.sortByDescending f xs else List.sortBy f xs
      let sort (xs: Person list) =
        match searchArgs.SortBy with
        | PersonSort.FirstName -> xs  |> sortF (fun p -> p.FirstName)
        | PersonSort.LastName -> xs  |> sortF (fun a -> a.LastName)
      return
        persons
        |> Map.toList
        |> List.map snd
        |> List.filter (fun p ->
            searchArgs.FirstName |> Option.map ((=) p.FirstName) |> Option.defaultValue true
            && searchArgs.LastName |> Option.map ((=) p.LastName) |> Option.defaultValue true
            && searchArgs.Twitter |> Option.map (Some >> (=) p.Twitter) |> Option.defaultValue true
            && searchArgs.Genders |> Option.map (List.map Some >> List.contains p.Gender) |> Option.defaultValue true
        )
        |> sort
        |> safeSkip searchArgs.Offset
        |> List.truncate searchArgs.Limit
    }

  let byId personId =
    async { return persons.TryFind personId }

  let authorForArticle articleId =
    async {
      return
        match articles.TryFind articleId with
        | None -> None
        | Some a -> persons.TryFind a.AuthorId
    }

  let authorForComment commentId =
    async {
      return
        match comments.TryFind commentId with
        | None -> None
        | Some c -> persons.TryFind c.AuthorId
    }

  let save (person: Person) =
    async { persons <- persons.Add(person.Id, person) }

  let delete personId =
    async { persons <- persons.Remove personId }


module Article =

  let search (searchArgs: ArticleSearchArgs) =
    async {
      let sortF f xs =
        if searchArgs.SortDescending then List.sortByDescending f xs else List.sortBy f xs
      let sort (xs: Article list) =
        match searchArgs.SortBy with
        | ArticleSort.Title -> xs  |> sortF (fun a -> a.Title)
        | ArticleSort.Created -> xs  |> sortF (fun a -> a.Created)
      return
        articles
        |> Map.toList
        |> List.map snd
        |> List.filter (fun a ->
            searchArgs.Title |> Option.map ((=) a.Title) |> Option.defaultValue true
            && searchArgs.Types |> Option.map (List.contains a.Type) |> Option.defaultValue true
            && searchArgs.CreatedAfter |> Option.map ((<=) a.Created) |> Option.defaultValue true
            && searchArgs.CreatedBefore |> Option.map ((>=) a.Created) |> Option.defaultValue true
        )
        |> sort
        |> safeSkip searchArgs.Offset
        |> List.truncate searchArgs.Limit
    }

  let byId articleId =
    async { return articles.TryFind articleId }

  let forComment commentId =
    async {
      return
        match comments.TryFind commentId with
        | None -> None
        | Some a -> articles.TryFind a.ArticleId
    }

  let allByAuthor authorId =
    async {
      return
        articles
        |> Map.toList
        |> List.map snd
        |> List.filter (fun a -> a.AuthorId = authorId)
    }

  let save (article: Article) =
    async { articles <- articles.Add(article.Id, article) }

  let delete articleId =
    async { articles <- articles.Remove articleId }


module Comment =

  let search (searchArgs: CommentSearchArgs) =
    async {
      let sortF f xs =
        if searchArgs.SortDescending then List.sortByDescending f xs else List.sortBy f xs
      let sort (xs: Comment list) =
        match searchArgs.SortBy with
        | CommentSort.Created -> xs |> sortF (fun c -> c.Created)
      return
        comments
        |> Map.toList
        |> List.map snd
        |> List.filter (fun c ->
            searchArgs.Author |> Option.map ((=) c.AuthorId) |> Option.defaultValue true
            && searchArgs.AuthorFirstName
               |> Option.map (fun fn -> persons.TryFind c.AuthorId |> Option.map (fun p -> p.FirstName) = Some fn)
               |> Option.defaultValue true
        )
        |> sort
        |> safeSkip searchArgs.Offset
        |> List.truncate searchArgs.Limit
    }

  let byId commentId =
    async { return comments.TryFind commentId }

  let allForArticle articleId =
    async {
      return
        comments
        |> Map.toList
        |> List.map snd
        |> List.filter (fun a -> a.ArticleId = articleId)
    }

  let save (comment: Comment) = 
    async { comments <- comments.Add(comment.Id, comment) }

  let delete commentId =
    async { comments <- comments.Remove commentId }
