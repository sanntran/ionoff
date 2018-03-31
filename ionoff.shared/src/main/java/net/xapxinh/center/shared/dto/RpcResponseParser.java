package net.xapxinh.center.shared.dto;

import java.util.ArrayList;
import java.util.List;

public final class RpcResponseParser {
	
	private RpcResponseParser() {
		// prevent installing
	}

	public static List<MediaFile> parseFileDtos(RpcResponseDto result) {
		List<MediaFile> objs = new ArrayList<MediaFile>();
		if (result != null) {
			ArrayListSerialDto dtos = (ArrayListSerialDto) result;
			for (int i = 0; i < dtos.size(); i++) {
				objs.add((MediaFile) dtos.get(i));
			}
		}
		return objs;
	}

	public static List<PlayerDto> parsePlayerDtos(RpcResponseDto result) {
		List<PlayerDto> objs = new ArrayList<PlayerDto>();
		if (result != null) {
			ArrayListSerialDto dtos = (ArrayListSerialDto) result;
			for (int i = 0; i < dtos.size(); i++) {
				objs.add((PlayerDto) dtos.get(i));
			}
		}
		return objs;
	}

	public static YoutubeVideos parseYoutubeVideoDtos(RpcResponseDto response) {
		SingleSerialDto serial = (SingleSerialDto) response;
		return (YoutubeVideos) serial.getSerializableDto();
	}

	public static List<Album> parseAlbumDtos(ArrayListSerialDto result) {
		List<Album> objs = new ArrayList<Album>();
		if (result != null) {
			ArrayListSerialDto dtos = (ArrayListSerialDto) result;
			for (int i = 0; i < dtos.size(); i++) {
				objs.add((Album) dtos.get(i));
			}
		}
		return objs;
	}

	public static List<Song> parseSongDtos(ArrayListSerialDto result) {
		List<Song> objs = new ArrayList<Song>();
		if (result != null) {
			ArrayListSerialDto dtos = (ArrayListSerialDto) result;
			for (int i = 0; i < dtos.size(); i++) {
				objs.add((Song) dtos.get(i));
			}
		}
		return objs;
	}
}
