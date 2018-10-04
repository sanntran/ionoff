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

	public static YoutubeVideosDto parseYoutubeVideoDtos(RpcResponseDto response) {
		SingleSerialDto serial = (SingleSerialDto) response;
		return (YoutubeVideosDto) serial.getSerializableDto();
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

	public static List<SongDto> parseSongDtos(ArrayListSerialDto result) {
		List<SongDto> objs = new ArrayList<SongDto>();
		if (result != null) {
			ArrayListSerialDto dtos = (ArrayListSerialDto) result;
			for (int i = 0; i < dtos.size(); i++) {
				objs.add((SongDto) dtos.get(i));
			}
		}
		return objs;
	}
}
