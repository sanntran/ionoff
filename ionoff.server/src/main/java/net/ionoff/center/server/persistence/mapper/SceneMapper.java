package net.ionoff.center.server.persistence.mapper;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.Scene;
import net.ionoff.center.server.entity.SceneAction;
import net.ionoff.center.server.entity.SceneDevice;
import net.ionoff.center.server.entity.ScenePlayerAction;
import net.ionoff.center.server.entity.SceneRelayAction;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.server.persistence.service.IZoneService;
import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.shared.dto.SceneActionDto;
import net.ionoff.center.shared.dto.SceneDeviceDto;
import net.ionoff.center.shared.dto.SceneDto;
import net.ionoff.center.shared.dto.ScenePlayerActionDto;
import net.ionoff.center.shared.dto.SceneRelayActionDto;

public class SceneMapper {

	@Autowired
	private DeviceMapper deviceMapper;
	
	public List<SceneDto> createSceneDtoList(List<Scene> scenes) {
		final List<SceneDto> sceneDtos = new ArrayList<SceneDto>();
		for (final Scene scene : scenes) {
			sceneDtos.add(createSceneDto(scene));
		}
		return sceneDtos;
	}
	
	public Scene createScene(SceneDto sceneDto, IZoneService zoneService) {
		final Scene scene = new Scene();
		updateScene(scene, sceneDto);
		final Zone zone = zoneService.findById(sceneDto.getZoneId());
		scene.setZone(zone);
		return scene;
	}
	
	public Scene updateScene(Scene scene, SceneDto sceneDto) {
		scene.setName(sceneDto.getName());
		scene.setOrder(sceneDto.getOrder());
		return scene;
	}

	public SceneDto createSceneDto(Scene scene) {
		final SceneDto sceneDto = new SceneDto();
		sceneDto.setId(scene.getId());
		sceneDto.setName(scene.getName());
		sceneDto.setOrder(scene.getOrder());
		if (scene.getTime() != null) {
			sceneDto.setTime(DateTimeUtil.yyyyMMddHHmmFormatter.format(scene.getTime()));
		}
		
		sceneDto.setZoneId(scene.getZone().getId());
		sceneDto.setZoneName(scene.getZone().getName());
		sceneDto.setProjectId(scene.getZone().getProject().getId());
		return sceneDto;
	}

	public SceneActionDto createSceneActionDto(SceneAction sceneAction) {
		final SceneActionDto sceneActionDto = newSceneActionDto(sceneAction);
		sceneActionDto.setId(sceneAction.getId());
		sceneActionDto.setAction(sceneAction.getAction());
		sceneActionDto.setSceneId(sceneAction.getSceneDevice().getScene().getId());
		sceneActionDto.setSceneName(sceneAction.getSceneDevice().getScene().getName());

		return sceneActionDto;
	}


	public List<SceneActionDto> createSceneActionDtoList(List<SceneAction> sceneActions) {
		final List<SceneActionDto> sceneActionDtos = new ArrayList<SceneActionDto>();
		if (sceneActions == null) {
			return sceneActionDtos;
		}
		for (final SceneAction sceneAction : sceneActions) {
			sceneActionDtos.add(createSceneActionDto(sceneAction));
		}
		Collections.sort(sceneActionDtos);
		return sceneActionDtos;
	}
	
	
	public SceneActionDto newSceneActionDto(SceneAction sceneAction) {

		if (sceneAction instanceof SceneRelayAction) {
			final SceneRelayAction sceneRelayAction = (SceneRelayAction)sceneAction;

			final SceneRelayActionDto sceneRelayActionDto = new SceneRelayActionDto();
			sceneRelayActionDto.setRelayId(sceneRelayAction.getRelay().getId());
			sceneRelayActionDto.setRelayName(sceneRelayAction.getRelay().getName());

			return sceneRelayActionDto;
		}
		else if (sceneAction instanceof ScenePlayerAction) {
			final ScenePlayerAction scenePlayerAction = (ScenePlayerAction)sceneAction;

			final ScenePlayerActionDto scenePlayerActionDto = new ScenePlayerActionDto();
			scenePlayerActionDto.setAlbum(scenePlayerAction.getAlbum());
			scenePlayerActionDto.setAlbumType(scenePlayerAction.getAlbumType());
			scenePlayerActionDto.setVolume(scenePlayerAction.getVolume());
			scenePlayerActionDto.setPlayerId(scenePlayerAction.getSceneDevice().getDevice().getId());
			scenePlayerActionDto.setPlayerName(scenePlayerAction.getSceneDevice().getDevice().getName());

			return scenePlayerActionDto;
		}
		return new SceneActionDto();
	}

	public SceneDeviceDto createSceneDeviceDto(SceneDevice sceneDevice) {
		final SceneDeviceDto sceneDeviceDto = new SceneDeviceDto();
		sceneDeviceDto.setId(sceneDevice.getId());
		sceneDeviceDto.setName(sceneDevice.getName());
		sceneDeviceDto.setOrder(sceneDevice.getOrder());
		sceneDeviceDto.setDuration(sceneDevice.getDuration());
		sceneDeviceDto.setDevice(deviceMapper.createDeviceDto(sceneDevice.getDevice()));
		final List<SceneActionDto> actionDtos = new ArrayList<SceneActionDto>();
		if (sceneDevice.getActions() != null && !sceneDevice.getActions().isEmpty()) {
			for (final SceneAction action : sceneDevice.getActions()) {
				actionDtos.add(createSceneActionDto(action));
			}
		}
		sceneDeviceDto.setActions(actionDtos);
		return sceneDeviceDto;
	}

}
