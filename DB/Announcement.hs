module DB.Announcement (getActiveAnnouncement, getAnnouncementForMember) where

import Models
import DB

import Database.PostgreSQL.Simple (Only(..))
import Control.Monad (liftM)
import Data.Maybe (listToMaybe)

rowToAnnouncement :: (AnnouncementId, Bool, String) -> Announcement
rowToAnnouncement (id, active, htmlStr) = Announcement id active htmlStr

rowsToMAnnouncement :: [(AnnouncementId, Bool, String)] -> Maybe Announcement
rowsToMAnnouncement = liftM rowToAnnouncement . listToMaybe

getActiveAnnouncement :: DatabaseM (Maybe Announcement)
getActiveAnnouncement =
  liftM rowsToMAnnouncement $ query_ "SELECT id, active, html FROM \"Announcements\" WHERE \"active\" = true"

getAnnouncementForMember :: Member -> DatabaseM (Maybe Announcement)
getAnnouncementForMember (Member idMember _ _ _ _) =
  liftM rowsToMAnnouncement $ query "SELECT \"Announcements\".id, \"Announcements\".active, \"Announcements\".html FROM \"Members\",\"Announcements\" WHERE \"Members\".id = ? AND \"Members\".\"gotActiveAnnouncement\" = false AND \"Announcements\".active = true" (Only idMember)
