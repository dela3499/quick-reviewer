import Html exposing (Html, Attribute, text, toElement, fromElement, div, span, input, textarea)
import Html.Attributes exposing (placeholder, value, style)
import Html.Events exposing (on, targetValue, onClick, onDoubleClick, onMouseDown)
import Signal exposing (Address)
import String
import Array exposing (Array)
import Json.Decode as Json

import StartApp as StartApp
import Dict exposing (Dict)
import Array exposing (Array)
import Effects exposing (Never)
import Set exposing (Set)
import Keyboard
import Task
import Time exposing (Time)
import Maybe

-- only add time if there's been an action, or screen touch, within the last few minutes.
-- maybe wait ten seconds before incrementing time for a post.
-- Would be good to go to top of page when changing pages.
-- Add toggles for highlight, sentence splitting, and light/dark
-- Add generative art behind title and navigation (really cool, but not distracting)
-- Add link to full text
-- Add progress and timer info, along with document metadata (time since publish date), length of document, estimated read time.
-- Add read TFIDF calculator and colormap
-- Need to add import, export capabilities
-- Home screen (with titles, metadata, and analytics.)
-- Doesn't show up properly on mobile.

commonWords = Set.fromList ["the", "of", "and", "to", "in", "a", "is", "that", "for", "it", "as", "was", "with", "be", "by", "on", "not", "he", "i", "this", "are", "or", "his", "from", "at", "which", "but", "have", "an", "had", "they", "you", "were", "their", "one", "all", "we", "can", "her", "has", "there", "been", "if", "more", "when", "will", "would", "who", "so", "no"]

getDocument model i = Array.get i model.documents |> Maybe.withDefault blankDocument

counter = Signal.map (\_ -> IncrementTimeSpent) (Time.every 1000)

app = StartApp.start
  { init = (initialModel, Effects.none)
  , view = view
  , update = update
  , inputs = [counter]} {-- Will want a timer here for the current time, and to timestamp actions. --}

main = app.html

type alias Model =
  { documents: Array Document
  , view: Page
  }
  
type Page
  = SingleDoc Int
  | AllDocs
  
type alias Document =
  { title: String
  , text: String
  , rating: Rating
  , summary: String
  , comment: String
  , timeSpent: Int -- seconds
  }

type Rating
  = Score Int
  | Unrated

type Action
 = Next
 | Previous
 | ShowAll
 | ShowOne Int
 | SetSummary Int String
 | SetComment Int String
 | SetRating Int Int
 | IncrementTimeSpent

initialDocuments: Array Document
initialDocuments =
  Array.fromList
    [ { title = "Single nucleotide polymorphisms linked to mitochondrial uncoupling protein genes UCP2 and UCP3 affect mitochondrial metabolism and healthy aging in female nonagenarians."
      , text = "Energy expenditure decreases with age, but in the oldest-old, energy demand for maintenance of body functions increases with declining health. Uncoupling proteins have profound impact on mitochondrial metabolic processes; therefore, we focused attention on mitochondrial uncoupling protein genes. Alongside resting metabolic rate (RMR), two SNPs in the promoter region of UCP2 were associated with healthy aging. These SNPs mark potential binding sites for several transcription factors; thus, they may affect expression of the gene. A third SNP in the 3'-UTR of UCP3 interacted with RMR. This UCP3 SNP is known to impact UCP3 expression in tissue culture cells, and it has been associated with body weight and mitochondrial energy metabolism. The significant main effects of the UCP2 SNPs and the interaction effect of the UCP3 SNP were also observed after controlling for fat-free mass (FFM) and physical-activity related energy consumption. The association of UCP2/3 with healthy aging was not found in males. Thus, our study provides evidence that the genetic risk factors for healthy aging differ in males and females, as expected from the differences in the phenotypes associated with healthy aging between the two sexes. It also has implications for how mitochondrial function changes during aging."
      , rating = Unrated
      , summary = ""
      , comment = ""
      , timeSpent = 0
      }
    , { title = "Loss of the integral nuclear envelope protein SUN1 induces alteration of nucleoli."
      , text = "A supervised machine learning algorithm, which is qualified for image classification and analyzing similarities, is based on multiple discriminative morphological features that are automatically assembled during the learning processes. The algorithm is suitable for population-based analysis of images of biological materials that are generally complex and heterogeneous. Here we used the algorithm wndchrm to quantify the effects on nucleolar morphology of the loss of the components of nuclear envelope in a human mammary epithelial cell line. The linker of nucleoskeleton and cytoskeleton (LINC) complex, an assembly of nuclear envelope proteins comprising mainly members of the SUN and nesprin families, connects the nuclear lamina and cytoskeletal filaments. The components of the LINC complex are markedly deficient in breast cancer tissues. We found that a reduction in the levels of SUN1, SUN2, and lamin A/C led to significant changes in morphologies that were computationally classified using wndchrm with approximately 100% accuracy. In particular, depletion of SUN1 caused nucleolar hypertrophy and reduced rRNA synthesis. Further, wndchrm revealed a consistent negative correlation between SUN1 expression and the size of nucleoli in human breast cancer tissues. Our unbiased morphological quantitation strategies using wndchrm revealed an unexpected link between the components of the LINC complex and the morphologies of nucleoli that serves as an indicator of the malignant phenotype of breast cancer cells."
      , rating = Unrated
      , summary = ""
      , comment = ""
      , timeSpent = 0
      }
    ]
    
blankDocument =
  { title = ""
  , text = ""
  , rating = Unrated
  , summary = ""
  , comment = ""
  , timeSpent = 0
  }

initialModel: Model
initialModel =
   { documents = initialDocuments
   , view = SingleDoc 0
   }

dt = 1

increment i = i + 1
decrement i = i - 1

--update: Action -> Model -> (Model, Effect.Effect Action)
update action model =
  let n = Array.length model.documents
      limit i = clamp 0 (n - 1) i
      changeViewDoc f =
        case model.view of
          SingleDoc i -> ({ model | view = SingleDoc (limit (f 1)) }, Effects.none)
          AllDocs -> (model, Effects.none)
  in
    case action of
      Next ->
        changeViewDoc increment

      Previous ->
        changeViewDoc decrement

      ShowAll ->
        ({ model | view = AllDocs }, Effects.none)
      
      ShowOne i ->
        ({ model | view = SingleDoc i }, Effects.none)
        
      SetSummary i summary -> -- There's a lot of repetition here. :(
        let newDocument =
          case Array.get i model.documents of
            Just document -> { document | summary = summary }
            Nothing -> blankDocument
        in
          ({ model | documents = Array.set i newDocument model.documents }, Effects.none)
           
      SetComment i comment ->
        let newDocument =
          case Array.get i model.documents of
            Just document -> { document | comment = comment }
            Nothing -> blankDocument
        in
          ({ model | documents = Array.set i newDocument model.documents }, Effects.none)
      
      SetRating i rating ->
        let newDocument =
          case Array.get i model.documents of
            Just document -> { document | rating = Score rating }
            Nothing -> blankDocument
        in
          ({ model | documents = Array.set i newDocument model.documents }, Effects.none)
          
      IncrementTimeSpent ->
        case model.view of
          SingleDoc i ->
            let newDocument =
              case Array.get i model.documents of
                Just document -> { document | timeSpent = document.timeSpent + dt }
                Nothing -> blankDocument
            in
              ({ model | documents = Array.set i newDocument model.documents }, Effects.none)
          
          AllDocs -> (model, Effects.none)
          

view: Address Action -> Model -> Html
view address model =
  case model.view of
    SingleDoc i -> viewSingleDoc address model i
    AllDocs -> viewAllDocs address model
  
viewSingleDoc address model i =
  let document = Array.get i model.documents |> Maybe.withDefault blankDocument
  in
    div
      [ singleDocStyle ]
      [ viewNavigation address model i
      --, viewProgressInfo 10 10 10 10
      , viewTitle document.title
      --, viewDocumentMetadata
      , viewText document.text
      , viewRatingInput address document.rating i
      , viewSummaryInput address document.summary i
      , viewCommentInput address document.comment i
      --, viewNavigation address model i
      --, text (toString model)
      ]

formatTimeSpent totalseconds =
  let minutes = totalseconds // 60
      seconds = totalseconds - (minutes * 60)
      space = if seconds < 10 then "0" else ""
  in (toString minutes) ++ ":" ++ space ++ (toString seconds)

viewNavigation address model i =
  let respond action = on "click" Json.value (\_ -> Signal.message address action)
      timeSpent = getDocument model i |> .timeSpent |> formatTimeSpent
      progress = (toString (i + 1)) ++ "/" ++ (toString (Array.length model.documents))
  in
    div
      [ viewNavigationStyle ]
      [ div
          [ previousStyle, respond Previous ]
          [ text "<" ]
      , div
          [ timeSpentStyle ]
          [ text timeSpent ]
      , div
          [ showAllStyle, respond ShowAll ]
          [ text "Home" ]
      , div
          [ timeSpentStyle ]
          [ text progress ]
      , div
          [ nextStyle, respond Next ]
          [ text ">" ]
      ]

viewProgressInfo totalTime documentTime i n =
  let show string =
        div
          [ style
            [ ("width", "33.3vw")
            , ("text-align", "center")
            , ("float", "left")]]
          [ text string ]
  in
    div
      [ progressInfoStyle ]
      [ show "14 minutes"
      , show "3 minutes"
      , show "Paper 4 of 40"
      ]

viewTitle title =
  div
    [ titleStyle ]
    [ text title ]

-- viewDocumentMetadata

viewText' string =
  div
    [ singleDocTextStyle ]
    [ text string ]

viewText string =
  let sentences =
        String.split "." string
        |> List.filter (\s -> String.length s > 0)
        |> List.map (\s -> viewSentence (s ++ "."))
  in
    div
      [ singleDocTextStyle ]
      sentences
      
viewSentence string =
  let words = String.split " " string
      colors = Array.fromList ["black", "darkblue", "darkgreen"]
      --getColor word = Array.get (String.length word % 3) colors |> Maybe.withDefault "black"
      isCommonWord word = Set.member word commonWords
      createElement word =
        span
          [ style
              --[ ("color", getColor word) ] ]
              [ ("opacity", if isCommonWord word then "0.7" else "1") ] ]
          [ text (word ++ " ") ]
          
      wordElements = List.map createElement words
  in
    div
      [ singleDocSentenceStyle ]
      wordElements
  
viewSummaryInput address summary i =
  textarea
    [ placeholder "What's the main idea?"
    , on "input" targetValue (\summary -> Signal.message address (SetSummary i summary))
    , value summary
    , summaryInputStyle
    ]
    []
    
viewCommentInput address comment i =
  textarea
    [ placeholder "Any comments or new ideas?"
    , on "input" targetValue (\comment -> Signal.message address (SetComment i comment))
    , value comment
    , commentInputStyle
    ]
    []
    
viewRatingInput address rating i =
  div
    [ ratingInputStyle ]
    [ div
        [ ratingInputHeaderStyle ]
        [ text "Will you act on these ideas soon?"]
    , div
        [ ratingOptionsStyle ]
        ( List.map (viewRatingOption address i rating) [1 .. 3] )
    ]
     
viewRatingOption address i rating label =
  let colors = Array.fromList ["red", "black", "green"]
      textLabels = Array.fromList ["Nope.", "Maybe.", "Yeah!"]
      textLabel = Array.get (label - 1) textLabels |> Maybe.withDefault ""
      color = Array.get (label - 1) colors |> Maybe.withDefault "black"
      emphasis =
        case rating of
          Score score -> score == label
          Unrated -> False
  in
    div
      [ ratingOptionStyle color emphasis
      , on "mousedown" Json.value (\_ -> Signal.message address (SetRating i label))
      ]
      [ text textLabel ]

viewAllDocs address model =
  let
    documentCards =
      List.map
        (viewDocumentCard address model)
        [0 .. (Array.length model.documents) - 1]
  in
    div
      [ allDocStyle ]
      ( viewBatchMetadata model :: documentCards)
   
viewBatchMetadata model =
  div
    [ batchMetadataStyle ]
    []
    
viewDocumentCard address model i =
  let document = Array.get i model.documents |> Maybe.withDefault blankDocument
  in
    div
      [ documentCardStyle ]
      [ text document.title ]


{-- Single-document styles --}

singleDocStyle =
  style
    [ ("top", "0em")
    , ("box-sizing", "border-box" )
    , ("background", "white")
    , ("position", "absolute")
    , ("font-size", "30px")
    ]

viewNavigationStyle =
  style
    [ ("position", "fixed")
    , ("width", "100%")
    , ("height", "25px")
    , ("background", "white")
    --, ("background", "#05444C")
    , ("z-index", "100")
    , ("border-bottom", "3px solid #097E8E")
    ]

buttonPadding = "0.2em"

previousStyle =
  style
    [ ("float", "left")
    , ("width", "10%")
    , ("box-sizing", "border-box" )
    , ("text-align", "center")
    , ("color", "#05444C")
    , ("padding", buttonPadding)
    , ("font-weight", "bold")
    , ("cursor", "pointer")
    , ("-webkit-user-select", "none")
    ]
    
nextStyle =
  style
    [ ("float", "right")
    , ("width", "10%")
    , ("box-sizing", "border-box" )
    , ("text-align", "center")
    , ("color", "#05444C")
    , ("padding", buttonPadding)
    , ("font-weight", "bold")
    , ("cursor", "pointer")
    , ("-webkit-user-select", "none")
    ]
    
showAllStyle =
  style
    [ ("float", "left")
    , ("width", "30%")
    , ("box-sizing", "border-box" )
    , ("text-align", "center")
    , ("color", "#05444C")
    , ("padding", buttonPadding)
    , ("font-weight", "bold")
    , ("cursor", "pointer")
    , ("-webkit-user-select", "none")
    ]
    
timeSpentStyle =
  style
    [ ("float", "left")
    , ("width", "25%")
    , ("box-sizing", "border-box" )
    , ("text-align", "center")
    , ("color", "black")
    , ("opacity", "0.6")
    , ("padding", buttonPadding)
    , ("-webkit-user-select", "none")
    ]
    
progressInfoStyle =
  style
    [ ("position", "fixed")
    , ("top", "2em")
    , ("padding", buttonPadding)
    ]

padding = "6px"

titleStyle =
  style
    [ ("margin-top", "25px")
    , ("font-size", "1em")
    , ("padding", "0.5em 0.3em 0.5em 0.3em")
    , ("background", "#097E8E")
    , ("color", "white")
    , ("box-sizing", "border-box")
    , ("display", "inline-block")
    , ("text-align", "left")
    , ("width", "100%")
    , ("border-bottom", "3px solid #097E8E")
    --, ("font-family", "sans-serif")
    --, ("font-weight", "bold")
    ]

singleDocSentenceStyle =
  style
    [ ("padding", padding)
    , ("padding-bottom", "0px")
    , ("background", "white")
    , ("text-align", "left")
    , ("font-size", "0.9em")
    ]

singleDocTextStyle =
  style
    [ ("padding-bottom", padding)
    , ("background", "white")
    , ("text-align", "left")
    , ("font-size", "0.9em")
    ]
    
summaryInputStyle =
  style
    [ ("width", "100%")
    , ("height", "10em")
    , ("background", "white")
    , ("border", "none")
    , ("border-top", "2px solid #097E8E")
    , ("padding", padding)
    , ("padding-top", "0.5em")
    , ("font-family", "sans-serif")
    , ("outline", "none")
    , ("margin", "0px 0px 0px 0px")
    , ("vertical-align", "bottom")
    , ("box-sizing", "border-box")
    , ("resize", "none")
    , ("overflow", "hidden")
    ]
    
commentInputStyle =
  style
    [ ("width", "100%")
    , ("height", "10em")
    , ("background", "white")
    , ("border", "none")
    , ("border-top", "2px solid #097E8E")
    , ("padding", padding)
    , ("padding-top", "0.5em")
    , ("font-family", "sans-serif")
    , ("outline", "none")
    , ("margin", "none")
    , ("box-sizing", "border-box")
    , ("resize", "none")
    , ("overflow", "hidden")
    , ("vertical-align", "bottom")
    ]

ratingInputStyle =
  style
    [ ("border-top", "2px solid #097E8E")
    , ("background", "white")
    , ("-webkit-user-select", "none")
    ]
    
ratingInputHeaderStyle =
  style
    [ ("padding", padding)
    --, ("padding-left", "0.7em")
    , ("padding-bottom", "0px")
    , ("background", "white")
    , ("font-size", "0.8em")
    , ("color", "grey")
    , ("font-family", "sans-serif")
    , ("box-sizing", "border-box")
    ]

ratingOptionsStyle =
  style
    [ ("border", "7px solid white")
    , ("background", "white")
    , ("float", "left")
    , ("width", "100%")
    , ("box-sizing", "border-box")
    ]

ratingOptionStyle color emphasis =
  style
    [ ("width", "33%")
    , ("display", "inline-block")
    , ("padding", padding)
    , ("box-sizing", "border-box")
    , ("text-align", "center")
    , ("background", color)
    , ("color", "white")
    , ("opacity", if emphasis then "0.7" else "0.4")
    , ("font-weight", if emphasis then "bold" else "normal")
    , ("border", "1px solid white")
    , ("cursor", "pointer")
    , ("-webkit-user-select", "none")
    ]

{-- All-documents styles --}

allDocStyle = style []
batchMetadataStyle = style []
documentCardStyle = style []
   
   
   
   
   
   
   
   
   
  