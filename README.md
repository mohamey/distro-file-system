# Haskell Distributed File Server
A distributed file system written in haskell using the servant library and based on the Andrew File System. The Server currently has the following features:

* FileServer and Client
* Directory Server
* Replication

The Distributed File System behaves like files are on a single file server, and the fragmentation of files being on different file servers is abstracted by the directory server.

## Requirements
* MongoDB
* Haskell
* Haskell-Stack

The system has been tested and works on Linux based Systems and OSX El Capitan.

## Usage
There are three stack projects in this repo:

* The Client
* The FileServer
* The DirectoryServer

To build them, run `stack build` in each of their respective directories. To run the project, you must first have a directory server running, then run at least one client. Then finally the client can be run.

### Directory Server
This requires a mongodb database be running before the directory server can be run. The Directory server can be run as follows:
```
stack exec DirectoryServer portNo
```

### File Server
This also requires a mongodb database to keep track of files present on the fileserver. The Fileserver is run as follows:
```
stack exec fileserver portNum DirectoryServerIP DirectoryServerPort
```

### Client
The client connects to the directory server to access files on any of the fileservers. The Client is run as follows:
```
stack exec client DirectoryServerIP DirectoryServerPort
```

The client has the following commands:
| Command | Description                                                                                            |
|---------|------------------------------------------------------------------------------------------------------- |
|list     |List all files available for access on the directory server                                             |
|get      |Get a file from the file system and print it out to the console                                         |
|put      |Given a local file that is **already** present on the file system, update it with your local copy       |
|post     |Upload a new file to the file system                                                                    |
|delete   |Delete a file present on the file system                                                                |
|open     |Open a file from the file system in vim, allow editing, then write changes back to file system on close |


## Project Features
The overall architecture of the project is kind of a nested Client Server model. The Directory Server acts only as a Server, and makes no requests. The File Server acts as both a client and a server. It is a server to the client, and is a client to the Directory Server. Finally, the Client queries both file servers and the Directory Server. The Directory Server never behaves like a client. 

### Directory Server
The Directory Server stores a list of files stored on all connected file servers and a list of connected file servers. It's main functions are that:

* It provides a list of files available for download
* It provides details of file locations for clients to access files
* It provides a list of file servers that have a secondary copy of a file

The usual flow of command is that the client requests a file and the fileserver resolves its location depending on the user requirements, meaning if the clients intended action requires write privileges then the directory server sends the location of the primary copy of the file. Otherwise, a random location of the secondary copy is returned. 

It is also responsible for returning file servers available to upload files to.

### File Server
The Fileservers are the actual storage servers of the file system, where uploaded files are kept. When a fileserver is run, it immediately uploads it's file listing to its specified directory server. The fileserver runs on top of it's own mongodb database, which keeps track of all the local files of the file server. Note, when deleting files not through the client be sure to delete both the actual file itself and its listing in the mongodb database. There is no limit to the number of fileservers that can be run at once.

### Replication
File replication across file servers is effectively managed using primary and secondary copies and AFS. This works by only allowing the primary copy of a file to be modified, and upon closing the primary copy of a file the changes are immediately written to all secondary copies on different file servers. The way this works is that the Directory Server keeps track of all duplicates of a file and allocates one as a primary and the rest as secondary.

When a file is closed, the fileserver hosting the primary copy immediately retrieves a list of file servers with a secondary copy from the Directory Server and uploads each instance of the secondary copy.

Please note, when running the system there will be problems deleting replicated files if two or more fileservers are sharing the same mongoDB database. This stems from the fact two file servers try to remove the same document from the database twice, so the second attempt fails.

## TODO
* ~~Coloured Terminal Prompts~~
* ~~Remove duplicate items from available files list on client~~
* ~~Auto retrieve list of files on the client from Directory Server~~
* ~~Auto update client list of files on post/delete~~
* ~~GET from fileserver should have a response type (Either ApiResponse FileObject)~~
* ~~New Type for (Either ApiResponse FileObject)~~
* ~~Fileservers should find out their own ip for use by Directory server~~ (Directory Server gets fileserver ip & port from request details)
* ~~Directory server should ensure duplicate files have the same timestamp as primary on entry~~
* Lock should be acquired before client can edit cached copy of file
