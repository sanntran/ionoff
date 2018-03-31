package net.xapxinh.center.server.servlet.response;

import net.xapxinh.center.server.entity.Player;
import net.xapxinh.center.shared.dto.PlayerDto;

public final class DtoFactory {
	private DtoFactory() {
		// prevent installing
	}
	public static PlayerDto createPlayerDto(Player player) {
		PlayerDto playerDto = new PlayerDto();
		playerDto.setId(player.getId());
		return playerDto;
	}
}
