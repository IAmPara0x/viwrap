module Viwrap.Pty (run, masterRead, slaveRead) where

import Control.Monad (when, void)
import System.Process (createProcess,shell, CreateProcess (..), StdStream (..), waitForProcess, ProcessHandle)
import System.Posix.Terminal
import System.Posix.IO (closeFd, fdToHandle,dupTo,stdInput,stdOutput,stdError)
import System.Posix.IO.ByteString (fdRead, fdWrite)
import Text.Printf (printf)
import System.Posix (Fd, ByteCount)
import Control.Concurrent (threadDelay)

buffer_size :: ByteCount
buffer_size = 2048

one_sec :: Int
one_sec = 1000000

run :: IO ()
run = do
    (masterFd, slaveFd) <- openPseudoTerminal
    getSlaveTerminalName masterFd >>= print
    getTerminalName masterFd >>= printf "master terminal name: %s\n"
    getTerminalName slaveFd >>= printf "slave terminal name: %s\n"
    getControllingTerminalName >>= printf "controlling terminal name: %s\n"


    ph <- forkProcess slaveFd
    masterRead masterFd

    void (fdWrite masterFd "this is slaveFd\n")
    waitForProcess ph
    masterRead masterFd

    -- slaveRead slaveFd

    mapM_ closeFd [masterFd]

forkProcess :: Fd -> IO ProcessHandle
forkProcess slaveFd = do
    sHandle <- fdToHandle slaveFd
    (_,_,_,ph) <- createProcess $ (shell "/tmp/hello.py")
                    { delegate_ctlc = True
                    , std_err = CreatePipe
                    , std_out = UseHandle sHandle
                    , std_in = UseHandle sHandle
                    }
    return ph

slaveRead :: Fd -> IO ()
slaveRead slaveFd = do
          threadDelay one_sec
          (content, count) <- fdRead slaveFd buffer_size
          when (count == 0) (slaveRead slaveFd)
          printf "slave content: %s\n" content

masterRead :: Fd -> IO ()
masterRead masterFd = do
           threadDelay one_sec
           (content, count) <- fdRead masterFd buffer_size
           when (count == 0) (masterRead masterFd)
           printf "master content: %s\n" content
