package net.ionoff.center.client.mediaplayer.rpc;

import net.ionoff.center.shared.dto.player.Album;
import net.ionoff.center.shared.dto.player.Command;
import net.ionoff.center.shared.dto.player.MediaFile;
import net.ionoff.center.shared.dto.player.MessageDto;
import net.ionoff.center.shared.dto.player.PlayListDto;
import net.ionoff.center.shared.dto.player.PlayNodeDto;
import net.ionoff.center.shared.dto.player.ScheduleDto;
import net.ionoff.center.shared.dto.player.StatusDto;
import net.ionoff.center.shared.dto.player.YoutubeVideosDto;
import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.RestService;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;
import java.util.List;

/**
 * @author Sann Tran
 */
public interface PlayerService extends RestService {
	@GET
	@Path("players/{playerId}/status")
	void getStatus(@PathParam("playerId") Long playerId,
			MethodCallback<StatusDto> callback);

	@GET
	@Path("players/{playerId}/playinglist")
	void getPlayingList(@PathParam("playerId") Long playerId,
			MethodCallback<PlayListDto> callback);

	@GET
	@Path("players/{playerId}/mediafiles")
	void getMediaFiles(@PathParam("playerId") Long playerId,
			@QueryParam("dir") String dir,
			MethodCallback<List<MediaFile>> callback);

	@GET
	@Path("players/{playerId}/mediafiles/refresh")
	void refreshMediaFiles(@PathParam("playerId") Long playerId,
			@QueryParam("dir") String dir,
			MethodCallback<List<MediaFile>> callback);
	
	@GET
	@Path("players/{playerId}/scheduler")
	void getSchedule(@PathParam("playerId") Long playerId,
			MethodCallback<ScheduleDto> callback);

	@POST
	@Path("players/{playerId}/scheduler")
	void updateSchedule(@PathParam("playerId") Long playerId, ScheduleDto schedule,
			MethodCallback<ScheduleDto> callback);

	@POST
	@Path("players/{playerId}/command")
	void sendCommand(@PathParam("playerId") Long playerId,
			Command command,
			MethodCallback<StatusDto> callback);

	@GET
	@Path("youtubevideos/search")
	void searchYoutubeVideos(@QueryParam("playerId") Long playerId,
			@QueryParam("searchKey") String searchKey,
			@QueryParam("pageToken") String pageToken,
			MethodCallback<YoutubeVideosDto> callback);

	@GET
	@Path("albums/search")
	void searchAlbums(@QueryParam("playerId") Long playerId,
			@QueryParam("searchKey") String searchKey,
			@QueryParam("searchScope") String searchScope,
			@QueryParam("pageNumber") Integer pageNumber,
			@QueryParam("pageSize") Integer pageSize,
			MethodCallback<List<Album>> callback);

	@GET
	@Path("albums/{albumId}")
	void getAlbum(
			@PathParam("albumId") Long albumId,
			@QueryParam("playerId") Long playerId,
			MethodCallback<Album> callback);

	@GET
	@Path("albums/special")
	void getSpecialAlbum(
			@QueryParam("playerId") Long playerId,
			MethodCallback<List<Album>> callback);
	
	@GET
	@Path("playlists")
	void getMyPlaylists(
			@QueryParam("playerId") Long playerId,
			MethodCallback<List<PlayListDto>> callback);
	
	@GET
	@Path("playlists/search")
	void getAllPlaylists(
			@QueryParam("playerId") Long playerId,
			@QueryParam("searchKey") String searchKey,
			@QueryParam("pageNumber") Integer pageNumber,
			@QueryParam("pageSize") Integer pageSize,
			MethodCallback<List<PlayListDto>> methodCallback);
	@GET
	@Path("playlists/{playlistId}/playnodes")
	void getPlayNodes(@PathParam("playlistId") Long playlistId, 
			@QueryParam("playerId") Long playerId, 
			MethodCallback<List<PlayNodeDto>> methodCallback);
	@PUT
	@Path("playlists")
	void insertPlayList(
			PlayListDto playingList,
			@QueryParam("playerId") Long playerId,
			MethodCallback<PlayListDto> methodCallback);
	
	@PUT
	@Path("playlists/{playlistId}")
	void updatePlayList(
			@PathParam("playlistId") Long playlistId, PlayListDto playingList, 
			@QueryParam("playerId") Long playerId,
			MethodCallback<PlayListDto> methodCallback);
	
	@DELETE
	@Path("playlists/{playlistId}")
	void deletePlaylist(
			@PathParam("playlistId") Long playlistId, 
			@QueryParam("playerId") Long playerId,
			MethodCallback<MessageDto> methodCallback);

	@DELETE
	@Path("playleafs/{playleafId}")
	void deletePlayLeaf(
			@PathParam("playleafId") Long playleafId, 
			@QueryParam("playerId") Long playerId,
			MethodCallback<MessageDto> methodCallback);
	
	@DELETE
	@Path("playnodes/{playnodeId}")
	void deletePlayNode(
			@PathParam("playnodeId") Long playnodeId, 
			@QueryParam("playerId") Long playerId,
			MethodCallback<MessageDto> methodCallback);
}
