package net.xapxinh.player.handler;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import net.xapxinh.player.EmbeddedMediaPlayerPanel;
import net.xapxinh.player.connection.MqttRequestMessage;
import net.xapxinh.player.model.PlayLeaf;
import net.xapxinh.player.model.PlayList;
import net.xapxinh.player.model.PlayNode;

public class PlaylistRequestHandler {
	
	private EmbeddedMediaPlayerPanel mediaPlayerPanel;
	private final Gson gson;
	
	public PlaylistRequestHandler(EmbeddedMediaPlayerPanel mediaPlayerPanel) {
		gson = new Gson();
		this.mediaPlayerPanel = mediaPlayerPanel;
	}

	public PlayList handleRequest(MqttRequestMessage request) {
		String command = request.getAsString("command");
		if (command == null) {
			return mediaPlayerPanel.getPlaylist();
		}
		if ("pl_update".equals(command)) {
			updatePlaylist(request);
		}
		return getPlaylist();
	}

	private void updatePlaylist(MqttRequestMessage request) {
		JsonElement plJson = request.getAsJsonElement("playlist");
		PlayList playlist = gson.fromJson(plJson, PlayList.class);
		PlayList playingList = getPlaylist();
		playingList.setId(playlist.getId());
		playingList.setName(playlist.getName());
		for (PlayNode node : playlist.getNodes()) {
			for (PlayNode playingNode : playingList.getNodes()) {
				if (playingNode.getIdx() == node.getIdx()) {
					updatePlayNode(playingNode, node);
				}
			}
		}
	}

	private void updatePlayNode(PlayNode playingNode, PlayNode node) {
		playingNode.setId(node.getId());
		for (PlayLeaf leaf : node.getLeafs()) {
			for (PlayLeaf playingLeaf : playingNode.getLeafs()) {
				if (playingLeaf.getIdx() == leaf.getIdx()) {
					playingLeaf.setId(leaf.getId());
				}
			}
		}
	}

	private PlayList getPlaylist() {
		return mediaPlayerPanel.getPlaylist();
	}
}
