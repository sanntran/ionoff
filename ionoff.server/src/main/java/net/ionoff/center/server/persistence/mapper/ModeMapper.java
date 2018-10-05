package net.ionoff.center.server.persistence.mapper;

import java.util.ArrayList;
import java.util.List;

import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.ModeScene;
import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.ModeSensorScene;
import net.ionoff.center.server.entity.ModeSensorUser;
import net.ionoff.center.server.entity.Scene;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ModeDto;
import net.ionoff.center.shared.dto.ModeSceneDto;
import net.ionoff.center.shared.dto.ModeSensorDto;
import net.ionoff.center.shared.dto.ModeSensorSceneDto;
import net.ionoff.center.shared.dto.ModeSensorUserDto;

public class ModeMapper {
	
	public Mode createMode(ModeDto modeDto, IProjectService projectService) {
		final Mode mode = new Mode();
		mode.setProject(projectService.findById(modeDto.getProjectId()));
		updateMode(mode, modeDto);
		return mode;
	}
	
	public Mode updateMode(Mode mode, ModeDto modeDto) {
		mode.setName(modeDto.getName());
		mode.setOrder(modeDto.getOrder());
		mode.setIsScheduled(modeDto.getIsScheduled());
		mode.setScheduleRepeat(modeDto.getScheduleRepeat());
		mode.setScheduleTime(modeDto.getScheduleTime());
		mode.setScheduleDay(modeDto.getScheduleDay());
		return mode;
	}

	public ModeDto createModeDto(Mode mode) {
		final ModeDto modeDto = new ModeDto();
		modeDto.setId(mode.getId());
		modeDto.setName(mode.getName());
		modeDto.setOrder(mode.getOrder());
		modeDto.setIsScheduled(mode.getIsScheduled());
		modeDto.setScheduleRepeat(mode.getScheduleRepeat());
		modeDto.setScheduleDay(mode.getScheduleDay());
		modeDto.setScheduleTime(mode.getScheduleTime());
		modeDto.setProjectId(mode.getProject().getId());
		modeDto.setProjectName(mode.getProject().getName());
		modeDto.setIsActivated(mode.getIsActivated());
		if (mode.getTime() != null) {
			modeDto.setTime(DateTimeUtil.ddMMHHmmFormatter.format(mode.getTime()));
		}
		if (mode.getScenes() != null) {
			final List<ModeSceneDto> modeSceneDtos = new ArrayList<ModeSceneDto>();
			for (final ModeScene modeScene : mode.getScenes()) {
				modeSceneDtos.add(createModeSceneDto(modeScene));
				modeDto.setScenes(modeSceneDtos);
			}
		}
		return modeDto;
	}

	public ModeSensorDto createModeSensorDto(ModeSensor modeSensor) {
		final ModeSensorDto modeSensorDto = new ModeSensorDto();
		modeSensorDto.setId(modeSensor.getId());
		modeSensorDto.setEnabled(modeSensor.getEnabled());
		modeSensorDto.setCondition(modeSensor.getCondition());
		modeSensorDto.setMessage(modeSensor.getMessage());
		if (modeSensor.getMode() != null) {
			modeSensorDto.setModeId(modeSensor.getMode().getId());
			modeSensorDto.setModeName(modeSensor.getMode().getName());
		}
		
		modeSensorDto.setSensorId(modeSensor.getSensor().getId());
		modeSensorDto.setSensorName(modeSensor.getSensor().getName());
		
		modeSensorDto.setScenes(new ArrayList<>());
		modeSensorDto.setUsers(new ArrayList<>());
		
		if (modeSensor.getScenes() != null) {
			for (ModeSensorScene scene : modeSensor.getScenes()) {
				modeSensorDto.getScenes().add(createModeSensorSceneDto(scene));
			}
		}
		if (modeSensor.getUsers() != null) {
			for (ModeSensorUser user : modeSensor.getUsers()) {
				modeSensorDto.getUsers().add(createModeSensorUserDto(user));
			}
		}
		return modeSensorDto;
	}

	public ModeSensorSceneDto createModeSensorSceneDto(ModeSensorScene modeSensorScene) {
		final ModeSensorSceneDto modeSensorSceneDto = new ModeSensorSceneDto();
		modeSensorSceneDto.setId(modeSensorScene.getId());
		modeSensorSceneDto.setModeSensorId(modeSensorScene.getModeSensor().getId());
		modeSensorSceneDto.setZoneId(modeSensorScene.getZone().getId());
		modeSensorSceneDto.setZoneName(modeSensorScene.getZone().getName());

		if (modeSensorScene.getScene() != null) {
			modeSensorSceneDto.setSceneId(modeSensorScene.getScene().getId());
			modeSensorSceneDto.setSceneName(modeSensorScene.getScene().getName());
		}
		if (modeSensorScene.getZone().getScenes() != null) {
			final List<String> sceneNameIds = new ArrayList<String>();
			for (final Scene scene : modeSensorScene.getZone().getScenes()) {
				sceneNameIds.add(BaseDto.formatNameID(scene.getName(), scene.getId()));
			}
			modeSensorSceneDto.setSceneNameIds(sceneNameIds);
		}

		return modeSensorSceneDto;
	}

	public ModeSceneDto createModeSceneDto(ModeScene modeScene) {
		final ModeSceneDto modeSceneDto = new ModeSceneDto();
		modeSceneDto.setId(modeScene.getId());
		modeSceneDto.setModeId(modeScene.getMode().getId());
		modeSceneDto.setZoneId(modeScene.getZone().getId());
		modeSceneDto.setZoneName(modeScene.getZone().getName());
		if (modeScene.getScene() != null) {
			modeSceneDto.setSceneId(modeScene.getScene().getId());
			modeSceneDto.setSceneName(modeScene.getScene().getName());
		}
		if (modeScene.getZone().getScenes() != null) {
			final List<String> sceneNameIds = new ArrayList<String>();
			for (final Scene scene : modeScene.getZone().getScenes()) {
				sceneNameIds.add(BaseDto.formatNameID(scene.getName(), scene.getId()));
			}
			modeSceneDto.setSceneNameIds(sceneNameIds);
		}
		return modeSceneDto;
	}
	

	public ModeSensorUserDto createModeSensorUserDto(
			ModeSensorUser modeSensorUser) {
		final ModeSensorUserDto modeSensorUserDto = new ModeSensorUserDto();
		modeSensorUserDto.setId(modeSensorUser.getId());
		modeSensorUserDto.setSendEmail(modeSensorUser.isSendEmail());
		modeSensorUserDto.setSendSms(modeSensorUser.isSendSms());
		modeSensorUserDto.setModeSensorId(modeSensorUser.getModeSensor().getId());
		modeSensorUserDto.setUserId(modeSensorUser.getUser().getId());
		modeSensorUserDto.setUserName(modeSensorUser.getUser().getName());
		return modeSensorUserDto;
	}
	

	public List<ModeDto> createModeDtoList(List<Mode> modes) {
		final List<ModeDto> modeDtos = new ArrayList<ModeDto>();
		for (final Mode mode : modes) {
			modeDtos.add(createModeDto(mode));
		}
		return modeDtos;
	}
	
}