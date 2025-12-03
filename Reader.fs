module Reader

let ReadLines filePath = System.IO.File.ReadLines filePath
let GetWriter filePath = System.IO.File.CreateText filePath
