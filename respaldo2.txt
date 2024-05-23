import System.IO
import System.Directory (renameFile, removeFile)
import Control.Exception (catch, IOException)
import Data.Time (getCurrentTime, utctDay, addDays, formatTime, defaultTimeLocale)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    putStrLn "Bienvenido a tu cuaderno de tareas."
    putStrLn "Escribe 'ver' para mostrar las tareas existentes, 'nueva' para añadir una nueva tarea, 'vercat' para ver tareas por categoría o 'eliminar' para eliminar una tarea."
    putStrLn "Escribe 'salir' para terminar."
    loop

loop :: IO ()
loop = do
    putStr "Escribe un comando: "
    hFlush stdout
    comando <- getLine
    case comando of
        "ver" -> do
            mostrarTareas Nothing
            loop
        "vercat" -> do
            categoria <- solicitarInputNoVacio "Escribe la categoría para filtrar las tareas: "
            mostrarTareas (Just categoria)
            loop
        "nueva" -> do
            agregarTarea
            loop
        "eliminar" -> do
            eliminarTarea
            loop
        "salir" -> do
            putStrLn "Cerrando el cuaderno de tareas. ¡Hasta luego!"
            return ()
        _ -> do
            putStrLn "Comando no reconocido. Por favor, escribe 'ver', 'vercat', 'nueva', 'eliminar' o 'salir'."
            loop

mostrarTareas :: Maybe String -> IO ()
mostrarTareas maybeCategoria = do
    contenido <- readFile "cuaderno.txt"
    let tareas = lines contenido
        tareasFiltradas = maybe tareas (\cat -> filtrarPorCategoria cat tareas) maybeCategoria
    putStrLn "Tareas guardadas:"
    mapM_ putStrLn (zipWith (\n tarea -> show n ++ ". " ++ tarea) [1..] tareasFiltradas)

solicitarInputNoVacio :: String -> IO String
solicitarInputNoVacio mensaje = do
    putStrLn mensaje
    input <- getLine
    if null input
        then do
            putStrLn "La entrada no puede estar vacía. Inténtalo de nuevo."
            solicitarInputNoVacio mensaje
        else return input

agregarTarea :: IO ()
agregarTarea = do
    categoria <- solicitarInputNoVacio "Escribe una categoría para la tarea: "
    tarea <- solicitarInputNoVacio "Escribe la tarea: "
    putStrLn "Escribe el nivel de prioridad (bajo, medio, alto): "
    prioridad <- getLine
    let prioridadFinal = if null prioridad then "medio" else prioridad
    putStrLn "Escribe la fecha de vencimiento (ej. 2024-05-31) o presiona Enter para asignar una semana a partir de hoy: "
    fechaVencimiento <- getLine
    fechaFinal <- if null fechaVencimiento
        then do
            currentTime <- getCurrentTime
            let currentDate = utctDay currentTime
            let dueDate = addDays 7 currentDate
            return (formatTime defaultTimeLocale "%Y-%m-%d" dueDate)
        else return fechaVencimiento
    appendFile "cuaderno.txt" (categoria ++ ": " ++ tarea ++ " - Prioridad: " ++ prioridadFinal ++ " - Vence: " ++ fechaFinal ++ "\n")
    putStrLn "Tarea añadida."

eliminarTarea :: IO ()
eliminarTarea = do
    contenido <- readFile "cuaderno.txt"
    let tareas = lines contenido
    putStrLn "Tareas guardadas:"
    mapM_ putStrLn (zipWith (\n tarea -> show n ++ ". " ++ tarea) [1..] tareas)
    numeroStr <- solicitarInputNoVacio "Escribe el número de la tarea que deseas eliminar: "
    let numero = read numeroStr :: Int
    let (antes, _:despues) = splitAt (numero - 1) tareas
    let nuevasTareas = unlines (antes ++ despues)
    writeFile "cuaderno_temp.txt" nuevasTareas
    renameFile "cuaderno_temp.txt" "cuaderno.txt"
    putStrLn "Tarea eliminada."

filtrarPorCategoria :: String -> [String] -> [String]
filtrarPorCategoria categoria tareas = filter (\tarea -> categoria `isPrefixOf` tarea) tareas
