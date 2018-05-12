CREATE SCHEMA `imedia` DEFAULT CHARACTER SET utf8 COLLATE utf8_unicode_ci ;
-- Import database

ALTER TABLE `imedia`.`albums_musicfiles` 
RENAME TO  `imedia`.`albums_songs` ;

ALTER TABLE `imedia`.`artists_musicfiles` 
RENAME TO  `imedia`.`artists_songs` ;

ALTER TABLE `imedia`.`musicfiles` 
RENAME TO  `imedia`.`songs` ;

ALTER TABLE `imedia`.`albums_songs` 
DROP FOREIGN KEY `albums_musicfiles_album_fk`,
DROP FOREIGN KEY `albums_musicfiles_musicfile_id`;
ALTER TABLE `imedia`.`albums_songs` 
CHANGE COLUMN `musicfile_id` `song_id` BIGINT(20) NOT NULL ,
CHANGE COLUMN `musicfile_index` `song_index` INT(11) NULL ;
ALTER TABLE `imedia`.`albums_songs` 
ADD CONSTRAINT `albums_songs_album_fk`
  FOREIGN KEY (`album_id`)
  REFERENCES `imedia`.`albums` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE,
ADD CONSTRAINT `albums_songs_song_id`
  FOREIGN KEY (`song_id`)
  REFERENCES `imedia`.`songs` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;
  
  
  ALTER TABLE `imedia`.`artists_songs` 
DROP FOREIGN KEY `artists_musicfiles_artist_fk`,
DROP FOREIGN KEY `artists_musicfiles_musicfile_fk`;
ALTER TABLE `imedia`.`artists_songs` 
CHANGE COLUMN `musicfile_id` `song_id` BIGINT(20) NOT NULL ,
DROP INDEX `artists_musicfiles_artist_fk_idx` ,
ADD INDEX `artists_songs_artist_fk_idx` (`artist_id` ASC),
DROP INDEX `artists_musicfiles_musicfile_fk_idx` ,
ADD INDEX `artists_songs_song_fk_idx` (`song_id` ASC);
ALTER TABLE `imedia`.`artists_songs` 
ADD CONSTRAINT `artists_songs_artist_fk`
  FOREIGN KEY (`artist_id`)
  REFERENCES `imedia`.`artists` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE,
ADD CONSTRAINT `artists_songs_song_fk`
  FOREIGN KEY (`song_id`)
  REFERENCES `imedia`.`songs` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;

  
ALTER TABLE `imedia`.`albums_songs` 
DROP INDEX `albums_musicfiles_album_fk_idx` ,
ADD INDEX `albums_songs_album_fk_idx` (`album_id` ASC),
DROP INDEX `albums_musicfiles_musicfile_id_idx` ,
ADD INDEX `albums_songs_song_id_idx` (`song_id` ASC);

ALTER TABLE `imedia`.`songs` 
DROP FOREIGN KEY `musicfiles_author_fk`;
ALTER TABLE `imedia`.`songs` 
DROP INDEX `musicfiles_author_fk_idx1` ,
ADD INDEX `songs_author_fk_idx` (`author_id` ASC);
ALTER TABLE `imedia`.`songs` 
ADD CONSTRAINT `songs_author_fk`
  FOREIGN KEY (`author_id`)
  REFERENCES `imedia`.`authors_` (`id`)
  ON DELETE SET NULL
  ON UPDATE CASCADE;
  
DROP TABLE `imedia`.`karaokes`;

ALTER TABLE `imedia`.`albums` 
ADD (`listen_count` INT(11) DEFAULT NULL, `uploaded_date` DATETIME DEFAULT NULL );
