;;; you may add alternate paths, in case one path ceases to exist.
;;; Useful if you have external SSDs and constantly mount and unmount them
;;; e.g. (music-directory . (#P "/media/some/ssd/path/Music/" #P "~/Documents/Music/"))
(defparameter *settings* '((documents . ("*.PDF" "*.pdf" "*.djvu"))
                              (images . ("*.PNG" "*.png" "*.jpg" "*.jpeg"))
                              (audio . ("*.wav" "*.WAV" "*.MP3" "*.mp3" "*.ogg" "*.OGG"))
                              (video . ("*.mp4" "*.MP4" "*.webm" "*.rm"))
                              (text . ("*.txt" "*.TXT" "*.md" "*.MD"))
                              (books-directory . (#P "~/Documents/Books/"))
                              (pictures-directory . (#P "~/Pictures/"))
                              (video-directory . (#P "~/Documents/Video/"))
                              (music-directory . (#P "~/Documents/Music/"))
                              (terminal . "st")
                              (player . ("nvlc" "ffplay"))
                              (video-player . ("vlc" "ffplay"))
                              (editor . "nvim")))

