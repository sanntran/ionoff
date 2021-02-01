/*
 * This file is part of VLCJ.
 *
 * VLCJ is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * VLCJ is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with VLCJ.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2015 Caprica Software Limited.
 */

package net.xapxinh.player;

import com.mpatric.mp3agic.*;
import it.sauronsoftware.jave.AudioAttributes;
import it.sauronsoftware.jave.Encoder;
import it.sauronsoftware.jave.EncoderException;
import it.sauronsoftware.jave.EncodingAttributes;

import java.io.File;
import java.io.IOException;

/**
 * Application entry-point.
 */
public class AppMain {

	public static void main(String[] args)
			throws InvalidDataException, IOException, UnsupportedTagException, NotSupportedException, EncoderException {
		//Mp3File mp3file = new Mp3File("F:\\media.ionoff.net\\media\\00a857c2f490111c\\1bfe0900796e649f.mp3");
//		if (mp3file.hasId3v2Tag()) {
//			ID3v2 id3v2Tag = mp3file.getId3v2Tag();
//			System.out.println("Track: " + id3v2Tag.getTrack());
//			System.out.println("Artist: " + id3v2Tag.getArtist());
//			System.out.println("Title: " + id3v2Tag.getTitle());
//			System.out.println("Album: " + id3v2Tag.getAlbum());
//			System.out.println("Year: " + id3v2Tag.getYear());
//			System.out.println("Genre: " + id3v2Tag.getGenre() + " (" + id3v2Tag.getGenreDescription() + ")");
//			System.out.println("Comment: " + id3v2Tag.getComment());
//			System.out.println("Lyrics: " + id3v2Tag.getLyrics());
//			System.out.println("Composer: " + id3v2Tag.getComposer());
//			System.out.println("Publisher: " + id3v2Tag.getPublisher());
//			System.out.println("Original artist: " + id3v2Tag.getOriginalArtist());
//			System.out.println("Album artist: " + id3v2Tag.getAlbumArtist());
//			System.out.println("Copyright: " + id3v2Tag.getCopyright());
//			System.out.println("URL: " + id3v2Tag.getUrl());
//			System.out.println("Encoder: " + id3v2Tag.getEncoder());
//			byte[] albumImageData = id3v2Tag.getAlbumImage();
//			if (albumImageData != null) {
//				System.out.println("Have album image data, length: " + albumImageData.length + " bytes");
//				System.out.println("Album image mime type: " + id3v2Tag.getAlbumImageMimeType());
//			}
//		}
//		if (mp3file.hasId3v1Tag()) {
//			mp3file.removeId3v1Tag();
//		}
//		if (mp3file.hasCustomTag()) {
//			mp3file.removeCustomTag();
//		}
//		if (mp3file.hasId3v2Tag()) {
//			mp3file.removeId3v2Tag();
//		}
//		ID3v2 id3v2Tag = new ID3v24Tag();
//		id3v2Tag.setTrack("1");
//		id3v2Tag.setArtist("Bằng Kiều / Trần Thu Hà");
//		id3v2Tag.setTitle("1");
//		id3v2Tag.setAlbum("Chiếc Lá Mùa Thu");
//		id3v2Tag.setComposer("Nguyễn Hưng");
//
//		mp3file.setId3v2Tag(id3v2Tag);
//		mp3file.save("F:\\media.ionoff.net\\media\\00a857c2f490111c\\1bfe0900796e649f_1.mp3");
		//
		convertToMp3("F:\\media.ionoff.net\\media\\b2da311e27192af9\\15a953f38f8c4817.flac",
				"F:\\media.ionoff.net\\media\\b2da311e27192af9\\15a953f38f8c4817.mp3");
	}


	private static void convertToMp3(String s, String t) throws EncoderException {

		File source = new File(s);
		File target = new File(t);
		AudioAttributes audio = new AudioAttributes();
		audio.setCodec("libmp3lame");
		audio.setBitRate(new Integer(320000));
		audio.setChannels(new Integer(2));
		audio.setSamplingRate(new Integer(44100));
		EncodingAttributes attrs = new EncodingAttributes();
		attrs.setFormat("mp3");
		attrs.setAudioAttributes(audio);
		Encoder encoder = new Encoder();
		encoder.encode(source, target, attrs);
	}

}
