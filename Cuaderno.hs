import System.IO
import System.Directory (renameFile, removeFile)
import Control.Exception (catch, IOException)
import Data.Time (getCurrentTime, utctDay, addDays, formatTime, defaultTimeLocale)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)

-- La función principal del programa
main :: IO ()
main = do
    -- Mostrar un mensaje de bienvenida al usuario
    putStrLn "Bienvenido a tu cuaderno de tareas."
    -- Instrucciones para que el usuario sepa cómo interactuar con el programa
    putStrLn "Escribe 'ver' para mostrar las tareas existentes, 'nueva' para añadir una nueva tarea, 'vercat' para ver tareas por categoría o 'eliminar' para eliminar una tarea."
    -- Instrucción para terminar el programa
    putStrLn "Escribe 'salir' para terminar."
    -- Llamar a la función loop para comenzar el bucle de interacción con el usuario
    loop


-- La función loop maneja el bucle principal del programa donde se espera el input del usuario
loop :: IO ()
loop = do
    -- Solicitar al usuario que escriba un comando
    putStr "Escribe un comando: "
    -- Forzar el vaciado del buffer de salida para asegurar que el mensaje se muestre inmediatamente
    hFlush stdout
    -- Leer el comando ingresado por el usuario
    comando <- getLine
    -- Manejar el comando ingresado usando una estructura case
    case comando of
        -- Si el comando es "ver", mostrar todas las tareas y repetir el bucle
        "ver" -> do
            mostrarTareas Nothing
            loop
        -- Si el comando es "vercat", solicitar la categoría, mostrar las tareas de esa categoría y repetir el bucle
        "vercat" -> do
            categoria <- solicitarInputNoVacio "Escribe la categoría para filtrar las tareas: "
            mostrarTareas (Just categoria)
            loop
        -- Si el comando es "nueva", agregar una nueva tarea y repetir el bucle
        "nueva" -> do
            agregarTarea
            loop
        -- Si el comando es "eliminar", eliminar una tarea y repetir el bucle
        "eliminar" -> do
            eliminarTarea
            loop
        -- Si el comando es "salir", mostrar un mensaje de despedida y terminar el programa
        "salir" -> do
            putStrLn "Cerrando el cuaderno de tareas. ¡Hasta luego!"
            return ()
        -- Si el comando no es reconocido, mostrar un mensaje de error y repetir el bucle
        _ -> do
            putStrLn "Comando no reconocido. Por favor, escribe 'ver', 'vercat', 'nueva', 'eliminar' o 'salir'."
            loop


-- La función mostrarTareas muestra las tareas almacenadas en el archivo "cuaderno.txt"
-- Si se proporciona una categoría, solo se muestran las tareas de esa categoría
mostrarTareas :: Maybe String -> IO ()
mostrarTareas maybeCategoria = do
    -- Leer el contenido del archivo "cuaderno.txt"
    contenido <- readFile "cuaderno.txt"
    -- Dividir el contenido en líneas individuales, cada una representando una tarea
    let tareas = lines contenido
        -- Filtrar las tareas según la categoría, si se proporciona una
        tareasFiltradas = maybe tareas (\cat -> filtrarPorCategoria cat tareas) maybeCategoria
    -- Mostrar un encabezado para la lista de tareas
    putStrLn "Tareas guardadas:"
    -- Imprimir cada tarea junto con su número de línea
    mapM_ putStrLn (zipWith (\n tarea -> show n ++ ". " ++ tarea) [1..] tareasFiltradas)


-- La función solicitarInputNoVacio pide al usuario que ingrese un valor no vacío
solicitarInputNoVacio :: String -> IO String
solicitarInputNoVacio mensaje = do
    -- Mostrar el mensaje al usuario
    putStrLn mensaje
    -- Leer la entrada del usuario
    input <- getLine
    -- Verificar si la entrada está vacía
    if null input
        then do
            -- Si la entrada está vacía, mostrar un mensaje de error y repetir la solicitud
            putStrLn "La entrada no puede estar vacía. Inténtalo de nuevo."
            solicitarInputNoVacio mensaje
        else
            -- Si la entrada no está vacía, devolverla
            return input


-- La función agregarTarea permite al usuario añadir una nueva tarea al cuaderno de tareas
agregarTarea :: IO ()
agregarTarea = do
    -- Solicitar al usuario que ingrese la categoría de la tarea
    categoria <- solicitarInputNoVacio "Escribe una categoría para la tarea: "
    -- Solicitar al usuario que ingrese la descripción de la tarea
    tarea <- solicitarInputNoVacio "Escribe la tarea: "
    -- Solicitar al usuario que ingrese el nivel de prioridad de la tarea
    putStrLn "Escribe el nivel de prioridad (bajo, medio, alto): "
    prioridad <- getLine
    -- Asignar "medio" como prioridad por defecto si no se ingresa ningún valor
    let prioridadFinal = if null prioridad then "medio" else prioridad
    -- Solicitar al usuario que ingrese la fecha de vencimiento de la tarea
    putStrLn "Escribe la fecha de vencimiento (ej. 2024-05-31) o presiona Enter para asignar una semana a partir de hoy: "
    fechaVencimiento <- getLine
    -- Si no se ingresa ninguna fecha, asignar una semana a partir de la fecha actual
    fechaFinal <- if null fechaVencimiento
        then do
            -- Obtener la fecha y hora actuales
            currentTime <- getCurrentTime
            let currentDate = utctDay currentTime
            -- Calcular la fecha de vencimiento sumando 7 días a la fecha actual
            let dueDate = addDays 7 currentDate
            -- Formatear la fecha de vencimiento en el formato "YYYY-MM-DD"
            return (formatTime defaultTimeLocale "%Y-%m-%d" dueDate)
        else return fechaVencimiento
    -- Añadir la tarea al archivo "cuaderno.txt"
    appendFile "cuaderno.txt" (categoria ++ ": " ++ tarea ++ " - Prioridad: " ++ prioridadFinal ++ " - Vence: " ++ fechaFinal ++ "\n")
    -- Mostrar un mensaje confirmando que la tarea ha sido añadida
    putStrLn "Tarea añadida."


-- La función eliminarTarea permite al usuario eliminar una tarea del cuaderno de tareas
eliminarTarea :: IO ()
eliminarTarea = do
    -- Leer el contenido del archivo "cuaderno.txt"
    contenido <- readFile "cuaderno.txt"
    -- Dividir el contenido en líneas individuales, cada una representando una tarea
    let tareas = lines contenido
    -- Mostrar las tareas con sus números correspondientes
    putStrLn "Tareas guardadas:"
    mapM_ putStrLn (zipWith (\n tarea -> show n ++ ". " ++ tarea) [1..] tareas)
    -- Solicitar al usuario que ingrese el número de la tarea que desea eliminar
    numeroStr <- solicitarInputNoVacio "Escribe el número de la tarea que deseas eliminar: "
    -- Convertir la entrada del usuario a un número entero
    let numero = read numeroStr :: Int
    -- Dividir la lista de tareas en dos partes: antes y después de la tarea a eliminar
    let (antes, _:despues) = splitAt (numero - 1) tareas
    -- Unir las listas antes y después de la tarea eliminada
    let nuevasTareas = unlines (antes ++ despues)
    -- Escribir las nuevas tareas en un archivo temporal
    writeFile "cuaderno_temp.txt" nuevasTareas
    -- Renombrar el archivo temporal para que reemplace al archivo original
    renameFile "cuaderno_temp.txt" "cuaderno.txt"
    -- Mostrar un mensaje confirmando que la tarea ha sido eliminada
    putStrLn "Tarea eliminada."


-- La función filtrarPorCategoria filtra las tareas que pertenecen a una categoría específica
filtrarPorCategoria :: String -> [String] -> [String]
filtrarPorCategoria categoria tareas = 
    -- Usar filter para seleccionar solo las tareas que comienzan con la categoría especificada
    filter (\tarea -> categoria `isPrefixOf` tarea) tareas

