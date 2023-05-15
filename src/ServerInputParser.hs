module ServerInputParser where

import Protolude hiding (Location, Down, Up, Dual, option, try)
import Data.Functor
import Control.Applicative
import Control.Monad
import Pipes.Attoparsec
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.Text as T
import Data.Attoparsec.ByteString
import Data.Text.Encoding
import qualified Data.ByteString as B
import Data.Word8
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.ByteString.Char8 as C8
import Event

data RemoteConsoleEvent = TelnetControlSeq | RemoteUserInput B.ByteString

serverInputParser :: A.Parser ServerEvent
serverInputParser =
  choice
    [ codepagePrompt
    , loginPrompt
    , passwordPrompt
    , welcomePrompt
    , postWelcome
    , locationParser
    , move
    , listEquipment
    , listInventory
    , itemStats
    , shopList
    , darkness
    , prompt
    , fightPrompt
    , obstacleEvent
    , cantGoDir
    , cantSeeTarget
    , darkInDirection
    , glanceDir
    , pickUp
    , youTook
    , mobGaveYouItem
    , gulp
    , eat
    , drinkFromAbsentObject
    , itemAbsent
    , liquidContainerIsEmpty
    , isNotHungry
    , isNotThirsty
    , examineContainer
    , expUp
    , myStats
    , imBashed
    , hitEvent
    , ripMob
    , checkNominative
    , checkGenitive
    , checkAccusative
    , checkDative
    , checkInstrumental
    , checkPrepositional
    , mobWentIn
    , mobWentOut
    , unknownMessage
    ]

checkNominative :: A.Parser ServerEvent
checkNominative = do cs >> string "1;30m"
                     string . encodeUtf8 $ "\"Храни тебя Господь, "
                     mob <- C.takeTill (== '!')
                     C.char '!'
                     string . encodeUtf8 $ "\" - перекрестили вы "
                     C.skipWhile (/= '.')
                     C.char '.'
                     clearColors
                     C.endOfLine
                     return . CheckNominative . ObjRef . decodeUtf8 $ mob

checkGenitive :: A.Parser ServerEvent
checkGenitive = do cs >> string "1;30m"
                   string . encodeUtf8 $ "Вы задумались над дальнейшей судьбой "
                   mob <- C.takeTill (== '.')
                   C.char '.'
                   clearColors
                   C.endOfLine
                   return . CheckGenitive . ObjRef . decodeUtf8 $ mob

checkAccusative :: A.Parser ServerEvent
checkAccusative = do cs >> string "1;30m"
                     string . encodeUtf8 $ "\"Ужас-то какой!..\" - подумали вы, со страхом глядя на "
                     mob <- C.takeTill (== '.')
                     C.char '.'
                     clearColors
                     C.endOfLine
                     return . CheckAccusative . ObjRef . decodeUtf8 $ mob

checkDative :: A.Parser ServerEvent
checkDative = do cs >> string "1;30m"
                 string . encodeUtf8 $ "\"За здоровье?\" - то ли спросили, то ли предложили вы "
                 mob <- C.takeTill (== '.')
                 C.char '.'
                 clearColors
                 C.endOfLine
                 return . CheckDative . ObjRef . decodeUtf8 $ mob

checkInstrumental :: A.Parser ServerEvent
checkInstrumental = do cs >> string "1;30m"
                       string . encodeUtf8 $ "Вы прихвастнули перед "
                       mob <- C.takeTill (== '.')
                       C.char '.'
                       clearColors
                       C.endOfLine
                       return . CheckInstrumental . ObjRef . decodeUtf8 $ mob

checkPrepositional :: A.Parser ServerEvent
checkPrepositional = do cs >> string "1;30m"
                        string . encodeUtf8 $ "Вы решили сосредоточить внимание на "
                        mob <- C.takeTill (== '.')
                        C.char '.'
                        clearColors
                        C.endOfLine
                        return . CheckPrepositional . ObjRef . decodeUtf8 $ mob

mobWentIn :: A.Parser ServerEvent
mobWentIn = do
  mob <- readWordsTillParser arrived
  C.space
  froms
  C.char '.'
  C.endOfLine
  return . MobWentIn . ObjRef . T.toLower . decodeUtf8 $ mob
  where
    froms =
      choice . fmap (string . encodeUtf8) $
      ["с севера", "с юга", "с запада", "с востока", "снизу", "сверху"]
    arrived =
      choice . fmap (string . encodeUtf8) $
      [ "пришел"
      , "пришла"
      , "пришло"
      , "пришли"
      , "прилетел"
      , "прилетела"
      , "прилетело"
      , "прилетели"
      , "прибежал"
      , "прибежала"
      , "прибежало"
      , "прибежали"
      , "приполз"
      , "приползла"
      , "приползло"
      , "приползли"
      , "приплыл"
      , "приплыла"
      , "приплыло"
      , "приплыли"
      ]

mobWentOut :: A.Parser ServerEvent
mobWentOut = do
  mob <- readWordsTillParser arrived
  C.space
  tos
  C.char '.'
  C.endOfLine
  return . MobWentOut . ObjRef . T.toLower . decodeUtf8 $ mob
  where
    tos =
      choice . fmap (string . encodeUtf8) $
      ["на север", "на юг", "на запад", "на восток", "вверх", "вниз"]
    arrived =
      choice . fmap (string . encodeUtf8) $
      [ "ушел"
      , "ушла"
      , "ушло"
      , "ушли"
      , "улетел"
      , "улетела"
      , "улетело"
      , "улетели"
      , "убежал"
      , "убежала"
      , "убежало"
      , "убежали"
      , "уполз"
      , "уползла"
      , "уползло"
      , "уползли"
      , "уплыл"
      , "уплыла"
      , "уплыло"
      , "уплыли"
      ]

codepagePrompt :: A.Parser ServerEvent
codepagePrompt = do
    iacWill
    iacWill
    iacWill
    C.endOfLine
    string "Online: "
    C.decimal
    C.endOfLine
    C.endOfLine
    string "Using keytable"
    _ <- manyTill' skipLine (string "Select one : ")
    return CodepagePrompt

iacWill :: A.Parser Word8
iacWill = do iac
             A.word8 251
             A.anyWord8

iacWont :: A.Parser Word8
iacWont = do iac
             wont
             A.anyWord8

iacDo :: A.Parser Word8
iacDo = do iac
           A.word8 doWord
           A.anyWord8

iacDont :: A.Parser Word8
iacDont = do iac
             A.word8 dontWord
             A.anyWord8

iacAny :: A.Parser Word8
iacAny = do iac
            A.anyWord8

remoteInputParser :: A.Parser ByteString
remoteInputParser = telnetControlSeq <|> utf8String
  where utf8String = do text <- A.takeWhile (not . C.isEndOfLine)
                        C.endOfLine
                        return $ text <> "\n"

telnetControlSeq :: A.Parser ByteString
telnetControlSeq = do iacWill <|> iacWont <|> iacDo <|> iacDont <|> iacAny >> remoteInputParser

loginPrompt :: A.Parser ServerEvent
loginPrompt = do
    string " --------"
    _ <- manyTill' skipLine (string $ encodeUtf8 "Введите имя персонажа (или \"новый\" для создания нового): ")
    return LoginPrompt

passwordPrompt :: A.Parser ServerEvent
passwordPrompt = do
    string $ encodeUtf8 "Персонаж с таким именем уже существует. Введите пароль : "
    iacGA
    return PasswordPrompt

myStats :: A.Parser ServerEvent
myStats = do -- header begin
  header
  row1
  row2
  row3
  row4
  row5
  row6
  row7
  row8
  row9
  maxHp <- row10
  maxMv <- row11
  return $ MyStats maxHp maxMv
    where colParser key valParser = do C.space >> cs >> colorCode >> string (encodeUtf8 key) >> C.char ':' >> C.skipWhile C.isSpace
                                       v <- valParser
                                       C.skipWhile C.isSpace
                                       return v
          leftFrame = C.space >> C.char '|' >> C.char '|'
          ncVDivider = C.skipWhile (== '-')
          vDivider = cs >> colorCode >> C.skipWhile (== '-')
          splitter = cs >> string "0;36m" >> C.char '|'
          nonColoredSplitter = C.char '|'
          rightFrame = cs >> string "0;36m" >> C.char '|' >> C.char '|'
          nonColoredRightFrame = C.char '|' >> C.char '|'
          colorCode = C.take 5
          strParser = decodeUtf8 <$> C.takeTill C.isSpace
          dblValParser = C.decimal >> C.char '(' >> many' C.space >> C.decimal >>= \v -> C.char ')' >> return v
          header = do
            C.space >> cs >> string "0;36m" >> C.skipWhile (== '-') >> C.endOfLine
            C.space >> C.char '|' >> C.char '|' >> C.space
            cs >> string "0;37m"
            string (encodeUtf8 "Вы ")
            myName <- C.takeTill (== ',')
            C.char ',' >> C.space
            myClass <- C.takeTill (== '.')
            C.char '.' >> C.skipWhile C.isSpace
            cs >> string "0;36m" >> C.char '|' >> C.char '|' >> C.endOfLine
            C.space >> C.skipWhile (== '-') >> C.endOfLine
          row1 = do leftFrame
                    colParser "Племя" strParser
                    splitter
                    colParser "Рост" dblValParser
                    splitter
                    armor <- colParser "Броня" C.decimal
                    splitter
                    colParser "Сопротивление" (return ())
                    rightFrame
                    C.endOfLine
          row2 = do leftFrame
                    colParser "Род" strParser
                    splitter
                    colParser "Вес" dblValParser
                    splitter
                    ac <- colParser "Защита" (C.signed C.decimal)
                    splitter
                    resistFire <- colParser "Огню" C.decimal
                    rightFrame
                    C.endOfLine
          row3 = do leftFrame
                    colParser "Вера" strParser
                    splitter
                    colParser "Размер" dblValParser
                    splitter
                    colParser "Поглощение" C.decimal
                    splitter
                    colParser "Воздуху" C.decimal
                    rightFrame
                    C.endOfLine
          row4 = do leftFrame
                    colParser "Уровень" $ cs >> colorCode >> C.decimal
                    splitter
                    colParser "Сила" dblValParser
                    splitter
                    colParser "Атака" (C.signed C.decimal)
                    splitter
                    colParser "Воде" C.decimal
                    rightFrame
                    C.endOfLine
          row5 = do leftFrame
                    colParser "Перевоплощений" $ cs >> colorCode >> C.decimal
                    splitter
                    colParser "Ловкость" dblValParser
                    splitter
                    colParser "Урон" C.decimal
                    splitter
                    colParser "Земле" C.decimal
                    rightFrame
                    C.endOfLine
          row6 = do leftFrame
                    colParser "Возраст" $ cs >> colorCode >> C.decimal
                    splitter
                    colParser "Телосложение" dblValParser
                    splitter
                    ncVDivider
                    nonColoredSplitter
                    colParser "Тьме" $ C.decimal >>= \v -> cs >> colorCode >> return v
                    nonColoredRightFrame
                    C.endOfLine
          row7 = do leftFrame
                    colParser "Опыт" $ cs >> colorCode >> C.decimal
                    splitter
                    colParser "Мудрость" dblValParser
                    splitter
                    colParser "Колдовство" C.decimal
                    splitter
                    vDivider
                    nonColoredRightFrame
                    C.endOfLine
          row8 = do leftFrame
                    colParser "ДСУ" $ cs >> colorCode >> C.decimal
                    splitter
                    colParser "Ум" dblValParser
                    splitter
                    colParser "Запоминание" C.decimal
                    splitter
                    colParser "Живучесть" C.decimal
                    rightFrame
                    C.endOfLine
          row9 = do leftFrame
                    colParser "Денег" $ cs >> colorCode >> C.decimal
                    splitter
                    colParser "Обаяние" dblValParser
                    splitter
                    ncVDivider
                    nonColoredSplitter
                    colParser "Разум" C.decimal
                    rightFrame
                    C.endOfLine
          row10 = do leftFrame
                     colParser "На счету" $ cs >> colorCode >> C.decimal
                     splitter
                     hp <- colParser "Жизнь" dblValParser
                     splitter
                     colParser "Воля" $ C.decimal >>= \v -> cs >> colorCode >> return v
                     nonColoredSplitter
                     colParser "Иммунитет" C.decimal
                     rightFrame
                     C.endOfLine
                     return hp
          row11 = do leftFrame
                     C.space >> cs >> colorCode >> skipWhile (/= telnetEscape)
                     splitter
                     mv <- colParser "Выносл." dblValParser
                     splitter
                     colParser "Здоровье" C.decimal
                     splitter
                     ncVDivider
                     nonColoredRightFrame
                     C.endOfLine
                     return mv

hitEvent :: A.Parser ServerEvent
hitEvent = do evt <- hit <|> miss
              C.endOfLine
              clearColors
              return evt
  where miss = do cs
                  F.foldl1 (<|>) . fmap string $ ["0;33m", "0;31m"]
                  evt <- choice [missHit1, missHit3]
                  C.char '.'
                  return evt
        missHit1 = do attacker <- readWordsTillParser (standardCasesParser "промазал")
                      string . encodeUtf8 $ ", когда"
                      standardCasesParser "хотел"
                      C.space
                      dmgTypeU
                      C.space
                      target <- C.takeTill (== '.')
                      return $ HitEvent (ObjRef . decodeUtf8 $ attacker) (ObjRef . decodeUtf8 $ target)
        missHit3 = do attacker <- readWordsTillParser (nonStandardCasesParser [ "попытался", "попыталась", "попыталось", "попытались" ])
                      C.space
                      dmgTypeU
                      C.space
                      target <- takeTill (\c -> c == _comma || C.isEndOfLine c)
                      C.char ','
                      m1 <|> m2 <|> m3
                      return $ HitEvent (ObjRef . decodeUtf8 $ attacker) (ObjRef . decodeUtf8 $ target)
                        where m1 = do string . encodeUtf8 $ " но не "
                                      standardCasesParser "рассчитал"
                                      string . encodeUtf8 $ " и "
                                      nonStandardCasesParser [ "промахнулся", "промахнулась", "промахнулось", "промахнулись" ]
                              m2 = do string . encodeUtf8 $ " но "
                                      nonStandardCasesParser ["его", "ее", "их"]
                                      string . encodeUtf8 $ " удар не достиг цели"
                              m3 = do string . encodeUtf8 $ " но "
                                      nonStandardCasesParser [ "промахнулся", "промахнулась", "промахнулось", "промахнулись" ]
        hit = do cs
                 F.foldl1 (<|>) . fmap string $ ["1;33m", "1;31m"]
                 attacker <- readWordsTillParser ((many' (dmgAmount >> C.space)) >> dmgType)
                 C.space
                 target <- choice [hit2, hit1]
                 return $ HitEvent (ObjRef . decodeUtf8 $ attacker) (ObjRef . decodeUtf8 $ target)
        hit1 = do
                  target <- C.takeTill (\c -> c == '.' || c == '!' || c == ',')
                  c <- C.anyChar
                  case c of '!' -> (string . encodeUtf8 $ " Теперь она никого не побеспокоит") >> return ()
                            ',' -> (string . encodeUtf8 $ " расколов ") >> nonStandardCasesParser ["его", "ее", "их"] >> (string . encodeUtf8 $ " жизнь в щепки") >> return ()
                            _ -> return ()
                  return target
        hit2 = do many' . string . encodeUtf8 $ "тело "
                  target <- readWordsTillParser (string . encodeUtf8 $ "и ")
                  nonStandardCasesParser ["она", "оно", "он"]
                  C.space
                  choice [h1, h2]
                  C.char '.'
                  return target
                    where h1 = do standardCasesParser "упал"
                                  C.space
                                  string . encodeUtf8 $ "на землю замертво"
                          h2 = do string . encodeUtf8 $ "с криком боли безжизненно "
                                  standardCasesParser "упал"
                                  string . encodeUtf8 $ " на землю"

        dmgAmount = F.foldl1 (<|>) . fmap (string . encodeUtf8) $ [ "легонько"
                                                                  , "слегка"
                                                                  , "очень сильно"
                                                                  , "чрезвычайно сильно"
                                                                  , "сильно"
                                                                  , "смертельно"
                                                                  ]
        standardCases base = fmap (base <>) ["а", "о", "и", ""]
        standardCasesParser = F.foldl1 (<|>) . fmap (string . encodeUtf8) . standardCases
        nonStandardCasesParser = choice . fmap (string . encodeUtf8)
        dmgType = F.foldl1 (<|>) . fmap (string . encodeUtf8) . concat . fmap standardCases $ [ "сокрушил"
                                                                                              , "ударил"
                                                                                              , "рубанул"
                                                                                              , "резанул"
                                                                                              , "пырнул"
                                                                                              , "огрел"
                                                                                              , "сокрушил"
                                                                                              , "уколол"
                                                                                              , "пронзил"
                                                                                              , "хлестнул"
                                                                                              , "ткнул"
                                                                                              , "ужалил"
                                                                                              , "лягнул"
                                                                                              ]

        dmgTypeU = F.foldl1 (<|>) . fmap (string . encodeUtf8) $ [ "сокрушить"
                                                                 , "ударить"
                                                                 , "рубануть"
                                                                 , "резануть"
                                                                 , "пырнуть"
                                                                 , "огреть"
                                                                 , "сокрушить"
                                                                 , "уколоть"
                                                                 , "пронзить"
                                                                 , "хлестнуть"
                                                                 , "ткнуть"
                                                                 , "ужалить"
                                                                 , "лягнуть"
                                                                 ]

iHitMob :: A.Parser ServerEvent
iHitMob = do cs >> string "1;33m"
             string . encodeUtf8 $ "Вы"
             C.space
             many' dmg
             F.foldl1 (<|>) . fmap (string . encodeUtf8) $ hitTypes
             C.space
             mob <- C.takeTill (== '.')
             C.char '.'
             C.endOfLine
             clearColors
             return . IHitMobEvent . ObjRef . decodeUtf8 $ mob
               where hitTypes = [ "сокрушили"
                        , "ударили"
                        , "рубанули"
                        , "резанули"
                        , "пырнули"
                        , "огрели"
                        , "сокрушили"
                        , "укололи"
                        , "пронзили"
                        , "хлестнули"
                        ]
                     dmg = tiny <|> small <|> strong
                     tiny = string . encodeUtf8 $ "легонько"
                     small = string . encodeUtf8 $ "слегка"
                     strong = string . encodeUtf8 $ "сильно"

imBashed :: A.Parser ServerEvent
imBashed = do many' clearColors
              cs
              string "1;31m"
              variant1 <|> variant2
              return ImBashedEvent
  where variant1 = do string (encodeUtf8 "Вы полетели на землю от мощного удара")
                      C.space
                      mob <- C.takeTill (== '.')
                      C.char '.'
                      return ()
        variant2 = do readWordsTill "вас на землю. Поднимайтесь!"
                      return ()

skipLine :: A.Parser ()
skipLine =
  skipWhile (not . C.isEndOfLine) *>
  C.endOfLine

expUp :: A.Parser ServerEvent
expUp = do
  string $ encodeUtf8 "Ваш опыт повысился на "
  C.decimal
  skipWhile (not . C.isEndOfLine)
  C.endOfLine
  pure ExpUpEvent

ripMob :: A.Parser ServerEvent
ripMob = do
  many' clearColors
  cs >> string "1;33m"
  choice [hit1, hit2]
  skipWhile (not . C.isEndOfLine)
  C.endOfLine
  clearColors
  partial <- manyTill' C.anyChar (string $ encodeUtf8 "душа медленно подымается в небеса.")
  C.endOfLine
  pure . MobRipEvent . ObjRef . T.toLower . L.head . T.splitOn " мертв" . fst . T.breakOnEnd "мертв" . decodeUtf8 . C8.pack $ partial
  where
    hit1 = string . encodeUtf8 $ "Вы выбили остатки жизни из"-- блохи своим мощным ударом."
    hit2 = string . encodeUtf8 $ "Вы нанесли "--"клопу прекрасный удар - после этого ему уже не встать."

isNotThirsty :: A.Parser ServerEvent
isNotThirsty = isFull <|> isOverFull >> return NotThirsty
  where isFull = string $ encodeUtf8 "Вы не чувствуете жажды."
        isOverFull = string $ encodeUtf8 "В вас больше не лезет."

isNotHungry :: A.Parser ServerEvent
isNotHungry = isFull <|> isOverFull >> return NotHungry
  where isFull = string $ encodeUtf8 "Вы наелись."
        isOverFull = string $ encodeUtf8 "Вы слишком сыты для этого!"

drinkFromAbsentObject :: A.Parser ServerEvent
drinkFromAbsentObject = do string $ encodeUtf8 "Вы не смогли это найти!"
                           return DrinkFromAbsentObject

liquidContainerIsEmpty :: A.Parser ServerEvent
liquidContainerIsEmpty = do string $ encodeUtf8 "Пусто."
                            return LiquidContainerIsEmpty

itemAbsent :: A.Parser ServerEvent
itemAbsent = do string $ encodeUtf8 "У вас нет '"
                item <- manyTill' C.anyChar (string $ encodeUtf8 "'.")
                return $ ItemAbsent (T.pack item)

eat :: A.Parser ServerEvent
eat = do string $ encodeUtf8 "Вы съели "
         food <- C.takeTill (== '.')
         C.char '.'
         return $ Eat (decodeUtf8 food)

gulp :: A.Parser ServerEvent
gulp = do string $ encodeUtf8 "Вы выпили "
          liquid <- manyTill' C.anyChar (string $ encodeUtf8 " из ")
          container <- C.takeTill (== '.')
          C.char '.'
          return $ Drink (T.pack liquid) (decodeUtf8 container)

examineContainer :: A.Parser ServerEvent
examineContainer = do
  skipWhile (not . C.isEndOfLine)
  C.endOfLine
  slots <- many1 armorSlot
  C.endOfLine
  containerAge
  C.endOfLine
  contName <- nameAndWhere
  C.endOfLine
  itms <- isEmpty <|> partiallyFilled
  return $ ExamineContainer (decodeUtf8 contName) itms
    where inAmmunition = string $ encodeUtf8 "в амуниции"
          inHands = string $ encodeUtf8 "в руках"
          isEmpty = do string $ encodeUtf8 " Внутри ничего нет."
                       return []
          wearAndTear = do C.endOfLine
                           _ <- readWordsTill " состоянии."
                           return ()
          containerAge = do string $ encodeUtf8 "Состояние: "
                            _ <- C.takeTill (== '.')
                            C.char '.'
          nameAndWhere = do contName <- C.takeTill (== '(')
                            C.char '('
                            inAmmunition <|> inHands
                            C.char ')'
                            return contName
          partiallyFilled = do string $ encodeUtf8 "Заполнен содержимым "
                               _ <- C.takeTill (== ':')
                               C.char ':'
                               C.endOfLine
                               many1 item
          item = singleItem <|> multipleItems
          multipleItems = do nom <- takeTill (\c -> c == _bracketleft || C.isEndOfLine c)
                             (C.char '[' >> return ()) <|> C.endOfLine
                             number <- C.decimal
                             C.char ']'
                             C.endOfLine
                             return $ Multiple (ObjRef . T.strip . decodeUtf8 $ nom) number
          singleItem = do nom <- takeTill (\c -> c == telnetEscape || C.isEndOfLine c)
                          state <- itemStateParser
                          C.endOfLine
                          return $ Single (ObjRef . T.strip . decodeUtf8 $ nom) state

welcomePrompt :: A.Parser ServerEvent
welcomePrompt = do
    C.endOfLine
    C.endOfLine
    string $ encodeUtf8 "Последний раз вы заходили к нам в"
    skipTillIACGA
    return WelcomePrompt

postWelcome :: A.Parser ServerEvent
postWelcome = do
    C.endOfLine
    string $ encodeUtf8 "  Добро пожаловать на землю Киевскую, богатую историей"
    A.count 4 skipLine
    return PostWelcome

glanceDir :: A.Parser ServerEvent
glanceDir = do cs >> "0;33m"
               dir <- direction
               A.word8 _colon
               cs >> "0;37m"
               C.space
               locTitle <- takeTill (C.isEndOfLine)
               C.endOfLine
               mobShortDescriptions <- (roomObjects "1;31m")
               clearColors
               let locationTitle = decodeUtf8 locTitle
                   mobs = ObjRef <$> mobShortDescriptions
                in return $ GlanceEvent dir locationTitle mobs

locationParser :: A.Parser ServerEvent
locationParser = do
  cs
  string "1;36m"
  locationName <- takeTill (== _bracketleft)
  A.word8 _bracketleft
  locId <- C.decimal
  A.word8 _bracketright
  cs
  string "0;37m"
  desc <- takeTill (== telnetEscape)
  many' schoolEntrance
  exits <- exitsParser
  _ <-
    many' $ do
      cs >> string "1;30m"
      string $ encodeUtf8 "Вы просто увязаете в грязи."
      cs >> string "0;37m"
      C.endOfLine
  _ <-
    many' $ do
      cs >> string "1;37m"
      string $ encodeUtf8 "Снежный ковер лежит у вас под ногами."
      cs >> string "0;37m"
      C.endOfLine
  _ <-
    many' $ do
      cs >> string "1;36m"
      string $ encodeUtf8 "Тоненький ледок вот-вот проломится под вами."
      cs >> string "0;37m"
      C.endOfLine
  _ <-
    many' $ do
      cs >> string "1;34m"
      string $ encodeUtf8 "У вас под ногами толстый лед."
      cs >> string "0;37m"
      C.endOfLine
  objects <- roomObjects "1;33m"
  mobs <- parseMobsInLocation
  clearColors
  zone <- parseZone 
  let location =
        Location
          { _locationId = locId
          , _locationTitle = T.strip $ decodeUtf8 locationName
          }
   in return $
      LocationEvent
        location
        (ObjRef <$> objects)
        mobs
        exits
        zone
  where
    schoolEntrance = do
      cs
      string $ encodeUtf8 "1;32mСовсем малых, да не"
      C.skipMany C.space
      string $ encodeUtf8 "обученных так и тянет "
      cs
      string $ encodeUtf8 "1;33mвойти "
      cs
      string $ encodeUtf8 "1;32mв "
      cs
      string $ encodeUtf8 "1;33mшколу"
      cs
      "1;32m."
      clearColors
      C.endOfLine
    exitsParser = do
      cs
      string "0;36m"
      A.word8 _bracketleft
      C.space
      string "Exits: "
      exits <- many' exitParser
      A.word8 _bracketright
      cs
      string "0;37m"
      C.endOfLine
      return exits
    exitParser = (openExit <|> closedExit) >>= \exit -> C.space >> return exit
      where
        openExit = dir >>= return . OpenExit
        closedExit =
          C.char '(' >> dir >>= \d -> C.char ')' >> return (ClosedExit d)
        dir = north <|> south <|> east <|> west <|> up <|> down
        north = C.char 'n' <|> C.char 'N' >> return North
        south = C.char 's' <|> C.char 'S' >> return South
        east = C.char 'e' <|> C.char 'E' >> return East
        west = C.char 'w' <|> C.char 'W' >> return West
        up = C.char 'd' <|> C.char 'D' >> return Down
        down = C.char 'u' <|> C.char 'U' >> return Up

roomObjects :: C8.ByteString -> A.Parser [Text]
roomObjects colorCode = do cs
                           string colorCode
                           fmap (T.dropWhileEnd (== '\r') . T.strip) . T.lines . decodeUtf8 <$> takeTill (== telnetEscape)

parseMobsInLocation :: A.Parser [ObjRef Mob InRoomDesc]
parseMobsInLocation =
  cs *> string "1;31m" *>
  (fmap ObjRef .
   L.filter
     (\line ->
        not $
        (T.isSuffixOf " сражается с ВАМИ!") line ||
        (T.isSuffixOf " отдыхает здесь.") line ||
        (T.isSuffixOf " лежит здесь, при смерти.") line ||
        (T.isSuffixOf " лежат здесь, при смерти.") line ||
        (T.isSuffixOf " лежит здесь, в обмороке.") line ||
        (T.isSuffixOf " лежат здесь, в обмороке.") line ||
        (T.isSuffixOf " выдает чье-то присутствие.") line) <$>
   many' parseMob)
  where
    parseMob :: A.Parser Text
    parseMob =
      (many' . string . encodeUtf8) "(летит) " *>
      C.takeWhile notEndOfLineOrControl >>= \mob ->
        C.endOfLine *> many' parseAffects *> (pure . T.strip . decodeUtf8) mob
    parseAffects =
      (choice . fmap (string . encodeUtf8))
        [ "...окружен воздушным, огненным, ледяным щитами"
        , "...окружен ледяным щитом"
        , "...окутан сверкающим коконом"
        , "...светится ярким сиянием"
        , "...серебристая аура"
        , "...слеп"
        , "...слепа"
        ] *>
      C.endOfLine
  --nonMobs = 

notEndOfLineOrControl c = c /= '\r' && c /= '\n' && c /= 'ÿ' && c /= '\ESC'

parseZone :: A.Parser (Maybe Text)
parseZone = listToMaybe <$> many' zoneLine

zoneLine :: A.Parser Text
zoneLine =
  C.endOfLine *>
  C.takeWhile (\c -> c /= '(' && notEndOfLineOrControl c) >>= \zoneName ->
    C.char '(' *>
    C.skipWhile (\c -> c /= ')' && notEndOfLineOrControl c) *>
    C.char ')' *>
    C.char '.' *>
    C.endOfLine *>
    (pure . T.strip . decodeUtf8) zoneName

move :: A.Parser ServerEvent
move = do
    string $ encodeUtf8 "Вы поплелись "
    option "" $ string $ encodeUtf8 "на "
    direction <- takeTill (== _period)
    A.word8 _period
    C.endOfLine
    return $ MoveEvent (decodeUtf8 direction)


clearColors :: A.Parser ()
clearColors = cs >> string "0;0m" >> return ()

cs :: A.Parser ()
cs = C.char '\ESC' >> C.char '[' >> return ()

telnetEscape :: Word8
telnetEscape = 0x1b

dblCrnl :: A.Parser ()
dblCrnl = do C.endOfLine
             C.endOfLine

darkness :: A.Parser ServerEvent
darkness = do string $ encodeUtf8 "Слишком темно..."
              return DarknessEvent

ansiColor :: A.Parser ()
ansiColor = do cs
               C.digit
               C.char ';'
               C.digit
               C.digit
               C.char 'm'
               return ()

cantGoDir :: A.Parser ServerEvent
cantGoDir = do string $ encodeUtf8 "Вы не сможете туда пройти..."
               return CantGoDir

cantSeeTarget :: A.Parser ServerEvent
cantSeeTarget =
  (string (encodeUtf8 "Вы не видите цели.") *> C.endOfLine) Data.Functor.$> CantSeeTarget

pickUp :: A.Parser ServerEvent
pickUp = do string $ encodeUtf8 "Вы подняли"
            C.space
            itemAccusative <- takeTill (== _period)
            A.word8 _period
            C.endOfLine
            return . PickItemEvent . ObjRef . decodeUtf8 $ itemAccusative

readWord :: A.Parser ByteString
readWord = do wrd <- takeTill (\x -> x == _period || x == _space || x == _cr || x == 10)
              w8 <- A.anyWord8
              if w8 == _space then return wrd
                              else fail "space expected"

readWordsTill :: Text -> A.Parser ByteString
readWordsTill str = do wrds <- manyTill' readWord (string $ encodeUtf8 str)
                       return $ C8.unwords wrds

readWordsTillParser :: A.Parser ByteString -> A.Parser ByteString
readWordsTillParser parser = do wrds <- manyTill' readWord parser
                                return $ C8.unwords wrds

mobGaveYouItem :: A.Parser ServerEvent
mobGaveYouItem = do mob <- readWordsTill "дал"
                    many' $ C.char 'а' <|> C.char 'о' <|> C.char 'и'
                    C.space
                    string $ encodeUtf8 "вам"
                    C.space
                    item <- takeTill (== _period)
                    A.word8 _period
                    C.endOfLine
                    return $ MobGaveYouItem (ObjRef . decodeUtf8 $ mob) (ObjRef . decodeUtf8 $ item)

youTook :: A.Parser ServerEvent
youTook = do
  string $ encodeUtf8 "Вы взяли"
  C.space
  lootCorpse <|> takeItemFromContainer <|> takeInRightHand <|> takeInLeftHand <|> takeInBothHands

lootCorpse :: A.Parser ServerEvent
lootCorpse = lootMoney <|> lootItem
  where lootMoney = lootOneCoin <|> lootPileCoins
        lootPileCoins = do tinyPile <|> smallPile <|> littlePile
                           C.space
                           string . encodeUtf8 $ "из трупа"
                           C.space
                           source <- takeTill (== _period)
                           A.word8 _period
                           C.endOfLine
                           return . LootMoney . ObjRef . decodeUtf8 $ source
        lootOneCoin = do string $ encodeUtf8 "одну куну"
                         C.space
                         string . encodeUtf8 $ "из трупа"
                         C.space
                         source <- takeTill (== _period)
                         A.word8 _period
                         C.endOfLine
                         return . LootMoney . ObjRef . decodeUtf8 $ source
        lootItem = do item <- readWordsTill "из трупа "
                      source <- takeTill (== _period)
                      A.word8 _period
                      C.endOfLine
                      return $ LootItem (ObjRef . decodeUtf8 $ item) (ObjRef . decodeUtf8 $ source)
        tinyPile = string . encodeUtf8 $ "малюсенькую горстку кун"
        smallPile = string . encodeUtf8 $ "небольшую горстку кун"
        littlePile = string . encodeUtf8 $ "маленькую кучку кун"

takeItemFromContainer :: A.Parser ServerEvent
takeItemFromContainer = do item <- readWordsTill "из "
                           container <- takeTill (== _period)
                           A.word8 _period
                           C.endOfLine
                           return $ TakeFromContainer (ObjRef . decodeUtf8 $ item) (ObjRef . decodeUtf8 $ container)

takeInLeftHand :: A.Parser ServerEvent
takeInLeftHand = do item <- readWordsTill "в левую руку"
                    A.word8 _period
                    C.endOfLine
                    return  $ TakeInLeftHand (ObjRef . decodeUtf8 $ item)

takeInRightHand :: A.Parser ServerEvent
takeInRightHand = do item <- readWordsTill "в правую руку"
                     A.word8 _period
                     C.endOfLine
                     return  $ TakeInRightHand (ObjRef . decodeUtf8 $ item)

takeInBothHands :: A.Parser ServerEvent
takeInBothHands = do string $ encodeUtf8 "Вы взяли"
                     C.space
                     item <- readWordsTill "в обе руки"
                     A.word8 _period
                     C.endOfLine
                     return  $ TakeInBothHands (ObjRef . decodeUtf8 $ item)

darkInDirection :: A.Parser ServerEvent
darkInDirection = do cs >> "0;33m"
                     dir <- direction
                     A.word8 _colon
                     cs >> "0;37m"
                     C.space
                     C.space
                     string $ encodeUtf8 "слишком темно."
                     return $ DarkInDirection dir

obstacleEvent :: A.Parser ServerEvent
obstacleEvent = do cs >> "0;33m"
                   dir <- direction
                   A.word8 _colon
                   cs >> "0;37m"
                   C.space
                   C.space
                   string $ encodeUtf8 "закрыто"
                   C.space
                   A.word8 _parenleft
                   many' $ string $ encodeUtf8 "вероятно "
                   obs <- takeTill (== _parenright)
                   A.word8 _parenright
                   return $ ObstacleEvent dir (decodeUtf8 obs)

direction :: A.Parser RoomDir
direction = north <|> south <|> east <|> west <|> up <|> down
  where north = do (string $ encodeUtf8 "север") <|> (string $ encodeUtf8 "Север")
                   return North
        south = do (string $ encodeUtf8 "юг") <|> (string $ encodeUtf8 "Юг")
                   return South
        east = do (string $ encodeUtf8 "восток") <|> (string $ encodeUtf8 "Восток")
                  return East
        west = do (string $ encodeUtf8 "запад") <|> (string $ encodeUtf8 "Запад")
                  return West
        up = do (string $ encodeUtf8 "вверх") <|> (string $ encodeUtf8 "Вверх")
                return Up
        down = do (string $ encodeUtf8 "вниз") <|> (string $ encodeUtf8 "Вниз")
                  return Down

prompt :: A.Parser ServerEvent
prompt = do C.endOfLine
            ansiColor
            hp <- C.decimal
            C.char 'H'
            ansiColor
            C.space
            ansiColor
            mv <- C.decimal
            C.char 'M'
            ansiColor
            C.space
            exp <- C.decimal
            string $ encodeUtf8 "о"
            C.space
            string $ encodeUtf8 "Зауч:0"
            C.space
            string $ encodeUtf8 "Вых:"
            C.skipWhile (/= '>')
            C.char '>'
            C.space
            iacGA
            return (PromptEvent hp mv)

fightPrompt :: A.Parser ServerEvent
fightPrompt = do
            C.endOfLine
            ansiColor
            hp <- C.decimal
            C.char 'H'
            ansiColor
            C.space
            ansiColor
            mv <- C.decimal
            C.char 'M'
            ansiColor
            C.space
            exp <- C.decimal
            string $ encodeUtf8 "о"
            C.space
            string $ encodeUtf8 "Зауч:0"
            C.space
            ansiColor
            C.char '['
            tankName <- C.takeWhile (/= ':')
            C.char ':'
            tankState <- C.takeWhile (/= ']')
            C.char ']'
            ansiColor
            C.space
            ansiColor
            C.char '['
            targetName <- C.takeWhile (/= ':')
            C.char ':'
            targetState <- C.takeWhile (/= ']')
            C.char ']'
            ansiColor
            C.space
            C.char '>'
            C.space
            iacGA
            return $ FightPromptEvent (ObjRef . decodeUtf8 $ tankName) (ObjRef . decodeUtf8 $ targetName)

shopList :: A.Parser ServerEvent
shopList = do skipMany shopHeadParser
              C.skipSpace
              C.decimal
              A.word8 _parenright
              C.skipSpace
              eitherP C.decimal (string $ encodeUtf8 "Навалом")
              C.skipSpace
              name <- itemNameParser
              C.skipSpace
              price <- C.decimal
              return $ ShopListItemEvent (ObjRef name) price

shopHeadParser :: A.Parser ()
shopHeadParser = do C.skipSpace
                    string "##"
                    C.skipSpace
                    string $ encodeUtf8 "Доступно"
                    C.skipSpace
                    string $ encodeUtf8 "Предмет"
                    C.skipSpace
                    string $ encodeUtf8 "Цена (куны)"
                    C.endOfLine
                    skipMany1 $ A.word8 _hyphen
                    C.endOfLine

itemStats :: A.Parser ServerEvent
itemStats = do string $ encodeUtf8 "Вы узнали следующее:"
               item <- weaponParser <|> armorParser
               return $ ItemStatsEvent item
                 where armorParser = do C.endOfLine
                                        name <- armorNameParser
                                        slots <- many1 armorSlot
                                        fiveLines
                                        C.endOfLine
                                        acVal <- acParser
                                        C.endOfLine
                                        armVal <- armorValParser
                                        return $ Armor (ObjRef name) slots (fromInteger acVal) (fromInteger armVal)
                       armorNameParser = do name <- itemNameParser
                                            string $ encodeUtf8 "БРОНЯ"
                                            return (decodeUtf8 name)
                       acParser = do cs
                                     string "0;37m"
                                     string $ encodeUtf8 "защита (AC) : "
                                     C.decimal
                       armorValParser = do string $ encodeUtf8 "броня       : "
                                           C.decimal
                       weaponParser = do C.endOfLine
                                         name <- weaponNameParser
                                         C.endOfLine
                                         weaponClass <- weaponClassParser
                                         slots <- many1 wpnSlot
                                         fiveLines
                                         C.endOfLine
                                         damageAvg <- damageAvgParser
                                         return $ Weapon (ObjRef name) weaponClass slots damageAvg
                       line = do C.endOfLine
                                 A.skipWhile (not . C.isEndOfLine)
                       wpnSlot = do C.endOfLine
                                    string $ encodeUtf8 "Можно взять в"
                                    C.skipSpace
                                    slot <- rh <|> lh <|> bh
                                    A.skipWhile (not . C.isEndOfLine)
                                    return slot
                       rh = do string $ encodeUtf8 "правую руку"
                               return Wield
                       lh = do string $ encodeUtf8 "левую руку"
                               return Hold
                       bh = do string $ encodeUtf8 "обе руки"
                               return DualWield
                       itemNameParser = do string $ encodeUtf8 "Предмет "
                                           A.word8 _quotedbl
                                           name <- takeTill (== _quotedbl)
                                           A.word8 _quotedbl
                                           string $ encodeUtf8 ", тип : "
                                           return name
                       weaponNameParser = do name <- itemNameParser
                                             string $ encodeUtf8 "ОРУЖИЕ"
                                             return (decodeUtf8 name)
                       weaponClassParser = do string $ encodeUtf8 "Принадлежит к классу"
                                              C.skipSpace
                                              A.word8 _quotedbl
                                              wc <- longBlade <|> axe <|> club <|> spear <|> dual <|> other
                                              A.word8 _quotedbl
                                              A.word8 _period
                                              return wc
                       longBlade = do string $ encodeUtf8 "длинные лезвия"
                                      return LongBlade
                       shortBlade = do string $ encodeUtf8 "короткие лезвия"
                                       return ShortBlade
                       dagger = do string $ encodeUtf8 "проникающее оружие"
                                   return Dagger
                       axe = do string $ encodeUtf8 "секиры"
                                return Axe
                       club = do string $ encodeUtf8 "палицы и дубины"
                                 return Club
                       spear = do string $ encodeUtf8 "копья и рогатины"
                                  return Spear
                       dual = do string $ encodeUtf8 "двуручники"
                                 return Dual
                       other = do string $ encodeUtf8 "иное оружие"
                                  return Other
                       damageAvgParser = do cs
                                            string $ encodeUtf8 "0;37mНаносимые повреждения"
                                            C.skipSpace
                                            C.skipWhile (not . C.isSpace)
                                            C.skipSpace
                                            string $ encodeUtf8 "среднее"
                                            C.skipSpace
                                            dmgAvg <- C.double
                                            A.word8 _period
                                            return dmgAvg
                       fiveLines = line >> line >> line >> line >> line


armorSlot = do C.endOfLine
               string $ encodeUtf8 "Можно"
               C.skipSpace
               slot <- generalSlot <|> feetSlot
               A.skipWhile (not . C.isEndOfLine)
               return slot
  where generalSlot = do string $ encodeUtf8 "надеть на"
                         C.skipSpace
                         body <|> waist <|> shoulders <|> hands <|> arms <|> head <|> legs
        feetSlot = do string $ encodeUtf8 "обуть"
                      return Feet
        body = do string $ encodeUtf8 "туловище"
                  return Body
        waist = do string $ encodeUtf8 "пояс"
                   return Waist
        hands = do string $ encodeUtf8 "кисти"
                   return Hands
        shoulders = do string $ encodeUtf8 "плечи"
                       return Shoulders
        arms = do string $ encodeUtf8 "руки"
                  return Arms
        head = do string $ encodeUtf8 "голову"
                  return Head
        legs = do string $ encodeUtf8 "ноги"
                  return Legs

listInventory :: A.Parser ServerEvent
listInventory = do string $ encodeUtf8 "Вы несете:"
                   list <- emptyInventory <|> many1 inventoryItem
                   return $ ListInventoryEvent list
                     where emptyInventory = do C.endOfLine
                                               C.skipSpace
                                               string $ encodeUtf8 "Вы ничего не несете."
                                               return []
                           inventoryItem = do C.endOfLine
                                              itemName <- itemNameParser
                                              itemState <- itemStateParser
                                              A.skipWhile (not . C.isEndOfLine)
                                              return (ObjRef itemName, itemState)

listEquipment :: A.Parser ServerEvent
listEquipment = do string $ encodeUtf8 "На вас надето:"
                   list <- emptyEquipList <|> many1 equipmentItem
                   return $ ListEquipmentEvent list
                     where emptyEquipList = do C.endOfLine
                                               C.skipSpace
                                               string $ encodeUtf8 "Вы голы, аки сокол."
                                               return []
                           bodyPart = body <|> head <|> legs <|> waist <|> rightHand <|> leftHand
                           body = do string $ encodeUtf8 "<на теле>"
                                     return Body
                           head = do string $ encodeUtf8 "<на голове>"
                                     return Head
                           legs = do string $ encodeUtf8 "<на ногах>"
                                     return Legs
                           waist = do string $ encodeUtf8 "<на поясе>"
                                      return Waist
                           rightHand = do string $ encodeUtf8 "<в правой руке>"
                                          return Wield
                           leftHand = do string $ encodeUtf8 "<в левой руке>"
                                         return Hold
                           equipmentItem = do C.endOfLine
                                              bp <- bodyPart
                                              skipMany1 C.space
                                              itemName <- itemNameParser
                                              state <- itemStateParser
                                              A.skipWhile (not . C.isEndOfLine)
                                              return (EquippedItem bp (ObjRef itemName), state)

itemNameParser :: A.Parser Text
itemNameParser = do itemName <- C.manyTill' C.anyChar (C.space *> C.space)
                    return (decodeUtf8 $ C8.pack itemName)

itemStateParser :: A.Parser ItemState
itemStateParser = stateParser "<великолепно>" Excellent
            <|> stateParser "<очень хорошо>" VeryGood
            <|> stateParser "<хорошо>" Good

stateParser stateTxt stateVal = do cs
                                   C.take 5
                                   string $ encodeUtf8 stateTxt
                                   cs
                                   string "0;37m"
                                   return stateVal

unknownMessage :: A.Parser ServerEvent
unknownMessage = takeTillEndOfLineOrGA >>= pure . UnknownServerEvent

iacGA :: A.Parser Word8
iacGA = do iac
           ga

takeTillEndOfLineOrGA :: A.Parser ByteString
takeTillEndOfLineOrGA = do txt <- takeTill (\w -> C.isEndOfLine w || w == iacWord)
                           eitherP C.endOfLine iacGA
                           skipMany $ A.word8 _cr
                           return txt

takeTillIACGA :: A.Parser B.ByteString
takeTillIACGA = do txt <- takeTill (== iacWord)
                   iacGA
                   return txt

skipTillIACGA :: A.Parser Word8
skipTillIACGA = do skipWhile (/= iacWord)
                   iacGA

iacWord :: Word8
iacWord = 255

wontWord :: Word8
wontWord = 252

doWord :: Word8
doWord = 253

dontWord :: Word8
dontWord = 254

wont :: A.Parser Word8
wont = A.word8 wontWord

iac :: A.Parser Word8
iac = A.word8 iacWord

ga :: A.Parser Word8
ga = A.word8 249
