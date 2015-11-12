-- | Курс функционального программирования (Осень'2015).
--
--   Пример полноценной программы на языке Haskell.
--   Программа моделирует карточную игру "Бура" по
--   упрощённым правилам.
--
import System.Random


-- | Количество очков, которое необходимо набрать для победы
winScore :: Int
winScore = 31

-- | Отладочный режим?
--   (влияет на вывод отладочной информации)
isDebug :: Bool
isDebug = False


-- Определения типов --------------------------------------------------------------------


-- | Все масти карт
data Suit = Clubs        -- ^ трефы / крести
          | Diamonds     -- ^ бубны
          | Hearts       -- ^ черви
          | Spades       -- ^ пики
          deriving (Show, Eq)


-- | Все значения карт
data Rank = R6 | R7 | R8 | R9 | R10 | Jack | Queen | King | Ace deriving (Show, Eq)


-- | Карта задаётся мастью и значением
data Card = Card {
    suit :: Suit,
    rank :: Rank }
    deriving Show


-- | Колода карт есть список карт
type Deck = [ Card ]


-- | Описание состояния игрового процесса
data GameState = GameState {
    activePlayer   :: ActivePlayer, -- ^ игрок, соверщающий действия на данном этапе
    dealer         :: ActivePlayer, -- ^ вистующий игрок
    humanDeck      :: Deck,         -- ^ карты на руках у человека
    computerDeck   :: Deck,         -- ^ карты на руках у компьютера
    restDeck       :: Deck,         -- ^ оставшиеся карты
    humanTricks    :: Deck,         -- ^ взятки человека
    computerTricks :: Deck,         -- ^ взятки компьютера
    trump          :: Suit,         -- ^ козырная масть
    playCard       :: [Card]        -- ^ текущая разыгрываемая карта: [] - нет карты, [c] - какая-то карта
    }


-- | Перечисление возможных игроков
data ActivePlayer = Human | Computer


-- | Перечисление состояний победы (как на отдельном ходе, так и во всей игре):
--   ещё никто не победил, победил человек, победил компьютер
data WhoIsWinner = Nobody | HumanWin | ComputerWin


-- | Ход игрока: номер карты из карт на руках
type Move = Int


-- Чистые функции -----------------------------------------------------------------------


-- | Начальная настройка игрового состояния
setup :: Deck -> GameState
setup deck =
    let humanDeck    = take 3 deck          -- первые три карты -- человеку
        computerDeck = take 3 (drop 3 deck) -- следующие три карты -- компьютеру
        restDeck     = drop 6 deck          -- оставшиеся карты
    in GameState Human Human humanDeck computerDeck restDeck [] [] (suit (head restDeck)) []


-- | Сделать ход (т.е. применить ход к игровому состоянию).
--   Вычисляется новое игровое состояние и определяется,
--   кто победил в результате этого хода.
applyMove :: Move -> GameState -> (GameState, WhoIsWinner)
applyMove move gs =
    -- рассмотрим пару (вистующий игрок, активный игрок) у текущего состояния
    case (dealer gs, activePlayer gs) of
        (Human, Human) ->
            -- человек ходит первым
            let (c,cs)  = extractCard (humanDeck gs) move -- вытащить карту с руки человека
                (rc:rs) = restDeck gs                     -- пара: карта из колоды, оставшаяся колода
                -- сформировать новое состояние игры:
            in (gs { activePlayer = Computer, -- теперь должен действовать компьютер
                     playCard = [c],          -- выложена карта человека
                     restDeck = rs,           -- из колоды извлекается карта...
                     humanDeck = rc:cs },     -- ...и помещается в руку человека
                Nobody)
        (Human, Computer) ->
            -- компьютер отвечает на карту человека
            let (compCard,cs) = extractCard (computerDeck gs) move -- вытащить карту с руки компьютера
                [humanCard]   = playCard gs                        -- текущая выложенная карта
                (rc:rs) = restDeck gs                              -- пара: карта из колоды, оставшаяся колода
                (gs', wt) = analyzeTrick humanCard compCard        -- определить результат боя (карта побита компьютером или нет)
                -- доформироать новое состояние игры:
            in (gs' { restDeck = rs,          -- из колоды извлекается карта...
                      computerDeck = rc:cs }, -- ...и помещается в руку компьютера
                wt)
        (Computer, Computer) ->
            -- компьютер ходит первым
            -- (комментарии аналогичны вышеприведённым)
            let (c,cs) = extractCard (computerDeck gs) move
                (rc:rs) = restDeck gs
            in (gs { activePlayer = Human, playCard = [c], computerDeck = rc:cs, restDeck = rs }, Nobody)
        (Computer, Human) ->
            -- человек отвечает на карту компьютера
            -- (комментарии аналогичны вышеприведённым)
            let (humanCard,cs) = extractCard (humanDeck gs) move
                [compCard] = playCard gs
                (rc:rs) = restDeck gs
                (gs', wt) = analyzeTrick humanCard compCard
            in (gs' { humanDeck = rc:cs, restDeck = rs }, wt)
    where
        -- ^ извлечь карту номер i из колоды; вернуть извлечённую карту и оставшуюся колоду
        extractCard cs i = (thisCard, before ++ after)
            where
                before           = take i cs
                (thisCard:after) = drop i cs
        -- ^ бьёт ли карта c1 карту c2 с учётом козырной масти (trump)
        isGreat trump c1 c2 =
            if suit c1 == trump then
                -- первая карта козырная
                if suit c2 == trump then
                    -- обе карты козырные, надо сравнить значения
                    rankCompare
                else
                    -- вторая -- не козырная, поэтому первая бьёт вторую
                    True
            else
                -- первая карта не козырная
                if suit c2 == trump then
                    -- а вторая -- козырная, поэтому бъёт первую
                    False
                else
                    -- обе карты не козырные: сравнение по значению карт
                    rankCompare
            where
                -- сравнение только по значению карт
                rankCompare = rankScore (rank c1) > rankScore (rank c2)
        -- ^ рассмотреть карты и определить, кто берёт взятку; вычислить новое состояние игры
        analyzeTrick humanCard compCard =
            if isGreat (trump gs) humanCard compCard then
                -- взятку берёт человек
                let ht = humanTricks gs
                in (gs { activePlayer = Human, -- поэтому ход переходит к нему
                         dealer = Human,
                         playCard = [],        -- на столе ничего не остаётся
                         humanTricks = humanCard:compCard:ht }, -- и побитые карты переходят во взятки человека
                    HumanWin)
            else
                -- взятку берёт компьютер
                -- (комментарии аналогичны)
                let ct = computerTricks gs
                in (gs { activePlayer = Computer, dealer = Computer, playCard = [], computerTricks = humanCard:compCard:ct }, ComputerWin)


-- | Определить, кто победитель по текущему состоянию игры
whoIsWinner :: GameState -> WhoIsWinner
whoIsWinner gs =
    -- посчитать взятые очки и определить победителя
    let humanScore    = deckScore (humanTricks gs)
        computerScore = deckScore (computerTricks gs)
    in
    if humanScore > computerScore then
        if humanScore >= winScore then
            HumanWin
        else
            Nobody
    else
        if computerScore >= winScore then
            ComputerWin
        else
            Nobody


-- | Вычислить стоимость набора карт
deckScore :: Deck -> Int
deckScore [] = 0
deckScore (c:cs) = rankScore (rank c) + deckScore cs


-- | Стоимости карт
rankScore :: Rank -> Int
rankScore R6    = 6
rankScore R7    = 7
rankScore R8    = 8
rankScore R9    = 9
rankScore R10   = 10
rankScore Jack  = 2
rankScore Queen = 3
rankScore King  = 4
rankScore Ace   = 11


-- | Вычислить ход компьютера.
--   Программирование AI является нетривиальной задачей, выходящей
--   за рамки данного курса, поэтому тут предложена простейшая
--   реализация: компьютер всегда выкладывает первую карту с руки
calcComputerMove :: GameState -> Move
calcComputerMove _gs = 0


-- | Сформировать строковое представление игры
showGameState :: GameState -> String
showGameState gs =
    "Козырь:                    " ++ showSuit (trump gs) ++
    ",\nКарты на руках:            " ++ showDeck (humanDeck gs) ++
    (if isDebug then (",\nКарты на руках компьютера: " ++ showDeck (computerDeck gs)) else "") ++
    ",\nВзятые карты игрока:       " ++ showDeck (humanTricks gs) ++ " (" ++ show (deckScore (humanTricks gs)) ++ " очков)" ++
    ",\nВзятые карты компьютера:   " ++ showDeck (computerTricks gs) ++ " (" ++ show (deckScore (computerTricks gs)) ++ " очков)" ++
    ",\nВ колоде осталось карт:    " ++ show (length (restDeck gs))


-- | Сформировать строковое представление списка карт
showDeck :: Deck -> String
showDeck [] = "пусто"
showDeck [c] = showCard c
showDeck (c:cs)  = showCard c ++ ", " ++ showDeck cs


-- | Сформировать строковое представление карты
showCard :: Card -> String
showCard c = showRank (rank c) ++ "/" ++ showSuit (suit c)


-- | Сформировать строковое представление масти
showSuit Clubs    = "трефы"
showSuit Diamonds = "бубны"
showSuit Hearts   = "черви"
showSuit Spades   = "пики"


-- | Сформировать строковое представление значения
showRank R6    = "6"
showRank R7    = "7"
showRank R8    = "8"
showRank R9    = "9"
showRank R10   = "10"
showRank Jack  = "Валет"
showRank Queen = "Дама"
showRank King  = "Король"
showRank Ace   = "Туз"


-- Функции с эффектами  -----------------------------------------------


-- | Основной игровой цикл:
--   - запрашивает ход у игрока,
--   - вычисляет новое состояние игры,
--   - если игра закончена, завершает работу,
--   - иначе повторить
gameLoop :: GameState -> IO ()
gameLoop state = do
    m <- inputMove state
    let (newState, whoTrick) = applyMove m state
    case whoTrick of
        ComputerWin -> putStrLn "Взятку забрал комьютер"
        HumanWin    -> putStrLn "Взятку забрал человек"
        Nobody      -> return ()
    case whoIsWinner newState of
        Nobody -> do
            gameLoop newState
        HumanWin -> do
            putStrLn "Вы выиграли!"
        ComputerWin -> do
            putStrLn "Вы проиграли! :("


-- | Ввести ход от активного игрока
inputMove :: GameState -> IO Move
inputMove gs = do
    case activePlayer gs of
        Human -> do
            putStrLn ""
            putStrLn (showGameState gs)
            putStr "Какой картой будете ходить? "
            cardNumberStr <- getLine
            let cardNumber = read cardNumberStr
            if cardNumber < 1 || cardNumber > 3 then do
                putStrLn "Неправильный ввод, введите число от 1 до 3"
                inputMove gs
            else
                return (cardNumber - 1) -- "внутри" игры карты пронумерованы от 0 до 2
        Computer -> do
            let cm = calcComputerMove gs
            putStrLn ("Компьютер походил картой: " ++ showCard (head (computerDeck gs)))
            return cm


-- | Перемешать колоду.
--   Перемешивание осуществляется так: взять первую карту и поменять её
--   с другой произвольной из колоды. Затем повторить со следующей и т.д.
shuffle :: Deck -> IO Deck
shuffle deck = do
    g <- newStdGen
    let r = randoms g
    return (shuffle' deck r)
    where
        shuffle' [] _ = []
        shuffle' [d] _ = [d]
        shuffle' (d:dk) (r:rs) =
            let r' = mod r (length dk)
                dkb = take r' dk
                (d':dka) = drop r' dk
            in d' : shuffle' (dkb ++ d : dka) rs


main = do
    -- сформировать начальное состояние колоды
    let initDeck = [ Card s r  | s <- [Clubs, Diamonds, Hearts, Spades], r <- [R6, R7, R8, R9, R10, Jack, Queen, King, Ace] ]
    -- перемешать её
    deck <- shuffle initDeck
    -- сформировать начальное состояние игры
    let initialState = setup deck
    -- запустить игровой процесс
    gameLoop initialState
