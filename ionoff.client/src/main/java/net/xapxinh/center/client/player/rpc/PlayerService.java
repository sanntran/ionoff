package net.xapxinh.center.client.player.rpc;

import java.util.List;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;

import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.RestService;

import net.xapxinh.center.shared.dto.Album;
import net.xapxinh.center.shared.dto.Command;
import net.xapxinh.center.shared.dto.MediaFile;
import net.xapxinh.center.shared.dto.MessageDto;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.PlayNodeDto;
import net.xapxinh.center.shared.dto.Schedule;
import net.xapxinh.center.shared.dto.Status;
import net.xapxinh.center.shared.dto.YoutubeVideos;

/**
 * @author Sann Tran
 */
public interface PlayerService extends RestService {
	@GET
	@Path("api/players/{playerId}/status")
	public void getStatus(@PathParam("playerId") Long playerId,
			MethodCallback<Status> callback);

	@GET
	@Path("api/players/{playerId}/playinglist")
	public void getPlayingList(@PathParam("playerId") Long playerId,
			MethodCallback<PlayListDto> callback);

	@GET
	@Path("api/players/{playerId}/mediafiles")
	public void getMediaFiles(@PathParam("playerId") Long playerId,
			@QueryParam("dir") String dir,
			MethodCallback<List<MediaFile>> callback);

	@GET
	@Path("api/players/{playerId}/mediafiles/refresh")
	public void refreshMediaFiles(@PathParam("playerId") Long playerId,
			@QueryParam("dir") String dir,
			MethodCallback<List<MediaFile>> callback);
	
	@GET
	@Path("api/players/{playerId}/schedule")
	public void getSchedule(@PathParam("playerId") Long playerId,
			MethodCallback<Schedule> callback);

	@POST
	@Path("api/players/{playerId}/schedule")
	public void updateSchedule(@PathParam("playerId") Long playerId, Schedule schedule,
			MethodCallback<Schedule> callback);

	@POST
	@Path("api/players/{playerId}/command")
	public void sendCommand(@PathParam("playerId") Long playerId,
			Command command,
			MethodCallback<Status> callback);

	@GET
	@Path("api/youtubevideos/search")
	public void searchYoutubeVideos(@QueryParam("playerId") Long playerId,
			@QueryParam("searchKey") String searchKey,
			@QueryParam("pageToken") String pageToken,
			MethodCallback<YoutubeVideos> callback);

	@GET
	@Path("api/albums/search")
	public void searchAlbums(@QueryParam("playerId") Long playerId,
			@QueryParam("searchKey") String searchKey,
			@QueryParam("searchScope") String searchScope,
			@QueryParam("pageNumber") Integer pageNumber,
			@QueryParam("pageSize") Integer pageSize,
			MethodCallback<List<Album>> callback);

	@GET
	@Path("api/albums/{albumId}")
	public void getAlbum(
			@PathParam("albumId") Long albumId,
			@QueryParam("playerId") Long playerId,
			MethodCallback<Album> callback);

	@GET
	@Path("api/albums/special")
	public void getSpecialAlbum(
			@QueryParam("playerId") Long playerId,
			MethodCallback<List<Album>> callback);
	
	@GET
	@Path("api/playlists")
	public void getMyPlaylists(
			@QueryParam("playerId") Long playerId,
			MethodCallback<List<PlayListDto>> callback);
	
	@GET
	@Path("api/playlists/search")
	public void getAllPlaylists(
			@QueryParam("playerId") Long playerId,
			@QueryParam("searchKey") String searchKey,
			@QueryParam("pageNumber") Integer pageNumber,
			@QueryParam("pageSize") Integer pageSize,
			MethodCallback<List<PlayListDto>> methodCallback);
	@GET
	@Path("api/playlists/{playlistId}/playnodes")
	public void getPlayNodes(@PathParam("playlistId") Long playlistId, 
			@QueryParam("playerId") Long playerId, 
			MethodCallback<List<PlayNodeDto>> methodCallback);
	@PUT
	@Path("api/playlists")
	public void insertPlayList(
			PlayListDto playingList,
			@QueryParam("playerId") Long playerId,
			MethodCallback<PlayListDto> methodCallback);
	
	@PUT
	@Path("api/playlists/{playlistId}")
	public void updatePlayList(
			@PathParam("playlistId") Long playlistId, PlayListDto playingList, 
			@QueryParam("playerId") Long playerId,
			MethodCallback<PlayListDto> methodCallback);
	
	@DELETE
	@Path("api/playlists/{playlistId}")
	public void deletePlaylist(
			@PathParam("playlistId") Long playlistId, 
			@QueryParam("playerId") Long playerId,
			MethodCallback<MessageDto> methodCallback);

	@DELETE
	@Path("api/playleafs/{playleafId}")
	public void deletePlayLeaf(
			@PathParam("playleafId") Long playleafId, 
			@QueryParam("playerId") Long playerId,
			MethodCallback<MessageDto> methodCallback);
	
	@DELETE
	@Path("api/playnodes/{playnodeId}")
	public void deletePlayNode(
			@PathParam("playnodeId") Long playnodeId, 
			@QueryParam("playerId") Long playerId,
			MethodCallback<MessageDto> methodCallback);
}
