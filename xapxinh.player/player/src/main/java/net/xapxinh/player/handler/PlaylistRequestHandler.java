package net.xapxinh.player.handler;

import java.util.Map;

import com.google.gson.Gson;

import net.xapxinh.player.EmbeddedMediaPlayerPanel;
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

	public PlayList handleRequest(Map<String, String> params) {
		
		if (params.isEmpty()) {
			return mediaPlayerPanel.getPlaylist();
		}
		String command = params.get("command");
		if (command == null) {
			return mediaPlayerPanel.getPlaylist();
		}
		
		if ("pl_update".equals(command)) {
			updatePlaylist(params);
		}
		return getPlaylist();
	}

	private void updatePlaylist(Map<String, String> params) {
		String plJson = params.get("playlist");
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
