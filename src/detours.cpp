/**
 * =============================================================================
 * CS2Fixes
 * Copyright (C) 2023 Source2ZE
 * =============================================================================
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License, version 3.0, as published by the
 * Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "cdetour.h"
#include "common.h"
#include "module.h"
#include "addresses.h"
#include "commands.h"
#include "detours.h"
#include "ctimer.h"
#include "irecipientfilter.h"
#include "entity/ccsplayercontroller.h"
#include "entity/ccsplayerpawn.h"
#include "entity/cbasemodelentity.h"
#include "entity/ccsweaponbase.h"
#include "entity/ctriggerpush.h"
#include "entity/cgamerules.h"
#include "entity/ctakedamageinfo.h"
#include "playermanager.h"
#include "igameevents.h"
#include "gameconfig.h"

#define VPROF_ENABLED
#include "tier0/vprof.h"

#include "tier0/memdbgon.h"

extern CGlobalVars *gpGlobals;
extern CEntitySystem *g_pEntitySystem;
extern IGameEventManager2 *g_gameEventManager;
extern CCSGameRules *g_pGameRules;

DECLARE_DETOUR(UTIL_SayTextFilter, Detour_UTIL_SayTextFilter);
DECLARE_DETOUR(UTIL_SayText2Filter, Detour_UTIL_SayText2Filter);
DECLARE_DETOUR(IsHearingClient, Detour_IsHearingClient);
DECLARE_DETOUR(CSoundEmitterSystem_EmitSound, Detour_CSoundEmitterSystem_EmitSound);
DECLARE_DETOUR(CCSWeaponBase_Spawn, Detour_CCSWeaponBase_Spawn);
DECLARE_DETOUR(TriggerPush_Touch, Detour_TriggerPush_Touch);
DECLARE_DETOUR(CGameRules_Constructor, Detour_CGameRules_Constructor);
DECLARE_DETOUR(CBaseEntity_TakeDamageOld, Detour_CBaseEntity_TakeDamageOld);
DECLARE_DETOUR(CNavMesh_GetNearestNavArea, Detour_CNavMesh_GetNearestNavArea);
DECLARE_DETOUR(CCSPlayer_MovementServices_TryPlayerMove, Detour_CCSPlayer_MovementServices_TryPlayerMove);

void FASTCALL Detour_CGameRules_Constructor(CGameRules *pThis)
{
	g_pGameRules = (CCSGameRules*)pThis;
	CGameRules_Constructor(pThis);
}

// CONVAR_TODO
static bool g_bBlockMolotoveSelfDmg = false;
static bool g_bBlockAllDamage = false;

CON_COMMAND_F(cs2f_block_molotov_self_dmg, "Whether to block self-damage from molotovs", FCVAR_LINKED_CONCOMMAND | FCVAR_SPONLY)
{
	if (args.ArgC() < 2)
		Msg("%s %i\n", args[0], g_bBlockMolotoveSelfDmg);
	else
		g_bBlockMolotoveSelfDmg = V_StringToBool(args[1], false);
}
CON_COMMAND_F(cs2f_block_all_dmg, "Whether to block all damage to players", FCVAR_LINKED_CONCOMMAND | FCVAR_SPONLY)
{
	if (args.ArgC() < 2)
		Msg("%s %i\n", args[0], g_bBlockAllDamage);
	else
		g_bBlockAllDamage = V_StringToBool(args[1], false);
}

void FASTCALL Detour_CBaseEntity_TakeDamageOld(Z_CBaseEntity *pThis, CTakeDamageInfo *inputInfo)
{
#ifdef _DEBUG
	Message("\n--------------------------------\n"
			"TakeDamage on %s\n"
			"Attacker: %s\n"
			"Inflictor: %s\n"
			"Ability: %s\n"
			"Damage: %.2f\n"
			"Damage Type: %i\n"
			"--------------------------------\n",
			pThis->GetClassname(),
			inputInfo->m_hAttacker.Get() ? inputInfo->m_hAttacker.Get()->GetClassname() : "NULL",
			inputInfo->m_hInflictor.Get() ? inputInfo->m_hInflictor.Get()->GetClassname() : "NULL",
			inputInfo->m_hAbility.Get() ? inputInfo->m_hAbility.Get()->GetClassname() : "NULL",
			inputInfo->m_flDamage,
			inputInfo->m_bitsDamageType);
#endif
	
	// Block all player damage if desired
	if (g_bBlockAllDamage && pThis->IsPawn())
		return;

	CBaseEntity *pInflictor = inputInfo->m_hInflictor.Get();
	const char *pszInflictorClass = pInflictor ? pInflictor->GetClassname() : "";

	// Prevent everything but nades from inflicting blast damage
	if (inputInfo->m_bitsDamageType == DamageTypes_t::DMG_BLAST && V_strncmp(pszInflictorClass, "hegrenade", 9))
		inputInfo->m_bitsDamageType = DamageTypes_t::DMG_GENERIC;

	// Prevent molly on self
	if (g_bBlockMolotoveSelfDmg && inputInfo->m_hAttacker == pThis && !V_strncmp(pszInflictorClass, "inferno", 7))
		return;

	CBaseEntity_TakeDamageOld(pThis, inputInfo);
}

// CONVAR_TODO
static bool g_bUseOldPush = false;

CON_COMMAND_F(cs2f_use_old_push, "Whether to use the old CSGO trigger_push behavior", FCVAR_LINKED_CONCOMMAND | FCVAR_SPONLY)
{
	if (args.ArgC() < 2)
		Msg("%s %i\n", args[0], g_bUseOldPush);
	else
		g_bUseOldPush = V_StringToBool(args[1], false);
}

void FASTCALL Detour_TriggerPush_Touch(CTriggerPush* pPush, Z_CBaseEntity* pOther)
{
	// This trigger pushes only once (and kills itself) or pushes only on StartTouch, both of which are fine already
	if (!g_bUseOldPush || pPush->m_spawnflags() & SF_TRIG_PUSH_ONCE || pPush->m_bTriggerOnStartTouch())
	{
		TriggerPush_Touch(pPush, pOther);
		return;
	}

	MoveType_t movetype = pOther->m_MoveType();

	// VPhysics handling doesn't need any changes
	if (movetype == MOVETYPE_VPHYSICS)
	{
		TriggerPush_Touch(pPush, pOther);
		return;
	}

	if (movetype == MOVETYPE_NONE || movetype == MOVETYPE_PUSH || movetype == MOVETYPE_NOCLIP)
		return;

	CCollisionProperty* collisionProp = pOther->m_pCollision();
	if (!IsSolid(collisionProp->m_nSolidType(), collisionProp->m_usSolidFlags()))
		return;

	if (!pPush->PassesTriggerFilters(pOther))
		return;

	if (pOther->m_CBodyComponent()->m_pSceneNode()->m_pParent())
		return;

	Vector vecAbsDir;

	matrix3x4_t mat = pPush->m_CBodyComponent()->m_pSceneNode()->EntityToWorldTransform();
	
	Vector pushDir = pPush->m_vecPushDirEntitySpace();

	// i had issues with vectorrotate on linux so i did it here
	vecAbsDir.x = pushDir.x * mat[0][0] + pushDir.y * mat[0][1] + pushDir.z * mat[0][2];
	vecAbsDir.y = pushDir.x * mat[1][0] + pushDir.y * mat[1][1] + pushDir.z * mat[1][2];
	vecAbsDir.z = pushDir.x * mat[2][0] + pushDir.y * mat[2][1] + pushDir.z * mat[2][2];

	Vector vecPush = vecAbsDir * pPush->m_flPushSpeed();

	uint32 flags = pOther->m_fFlags();

	if (flags & (FL_BASEVELOCITY))
	{
		vecPush = vecPush + pOther->m_vecBaseVelocity();
	}

	if (vecPush.z > 0 && (flags & FL_ONGROUND))
	{
		addresses::SetGroundEntity(pOther, nullptr);
		Vector origin = pOther->GetAbsOrigin();
		origin.z += 1.0f;

		pOther->Teleport(&origin, nullptr, nullptr);
	}

	pOther->m_vecBaseVelocity(vecPush);

	flags |= (FL_BASEVELOCITY);
	pOther->m_fFlags(flags);
}

void FASTCALL Detour_CCSWeaponBase_Spawn(CBaseEntity *pThis, void *a2)
{
#ifdef _DEBUG
	const char *pszClassName = pThis->m_pEntity->m_designerName.String();
	Message("Weapon spawn: %s\n", pszClassName);
#endif

	CCSWeaponBase_Spawn(pThis, a2);
}

void FASTCALL Detour_CSoundEmitterSystem_EmitSound(ISoundEmitterSystemBase *pSoundEmitterSystem, CEntityIndex *a2, IRecipientFilter &filter, uint32 a4, void *a5)
{
	//ConMsg("Detour_CSoundEmitterSystem_EmitSound\n");
	CSoundEmitterSystem_EmitSound(pSoundEmitterSystem, a2, filter, a4, a5);
}

bool FASTCALL Detour_IsHearingClient(void* serverClient, int index)
{
	ZEPlayer* player = g_playerManager->GetPlayer(index);
	if (player && player->IsMuted())
		return false;

	return IsHearingClient(serverClient, index);
}

void SayChatMessageWithTimer(IRecipientFilter &filter, const char *pText, CCSPlayerController *pPlayer, uint64 eMessageType)
{
	char buf[256];

	// Filter console message - remove non-alphanumeric chars and convert to lowercase
	uint32 uiTextLength = strlen(pText);
	uint32 uiFilteredTextLength = 0;
	char filteredText[256];

	for (uint32 i = 0; i < uiTextLength; i++)
	{
		if (pText[i] >= 'A' && pText[i] <= 'Z')
			filteredText[uiFilteredTextLength++] = pText[i] + 32;
		if (pText[i] == ' ' || (pText[i] >= '0' && pText[i] <= '9') || (pText[i] >= 'a' && pText[i] <= 'z'))
			filteredText[uiFilteredTextLength++] = pText[i];
	}
	filteredText[uiFilteredTextLength] = '\0';

	// Split console message into words seperated by the space character
	CUtlVector<char*, CUtlMemory<char*, int>> words;
	V_SplitString(filteredText, " ", words);

	//Word count includes the first word "Console:" at index 0, first relevant word is at index 1
	int iWordCount = words.Count();
	uint32 uiTriggerTimerLength = 0;

	if (iWordCount == 2)
		uiTriggerTimerLength = V_StringToUint32(words.Element(1), 0, NULL, NULL, PARSING_FLAG_SKIP_WARNING);

	for (int i = 1; i < iWordCount && uiTriggerTimerLength == 0; i++)
	{
		uint32 uiCurrentValue = V_StringToUint32(words.Element(i), 0, NULL, NULL, PARSING_FLAG_SKIP_WARNING);
		uint32 uiNextWordLength = 0;
		char* pNextWord = NULL;

		if (i + 1 < iWordCount)
		{
			pNextWord = words.Element(i + 1);
			uiNextWordLength = strlen(pNextWord);
		}

		// Case: ... X sec(onds) ... or ... X min(utes) ...
		if (pNextWord != NULL && uiNextWordLength > 2 && uiCurrentValue > 0)
		{
			if (pNextWord[0] == 's' && pNextWord[1] == 'e' && pNextWord[2] == 'c')
				uiTriggerTimerLength = uiCurrentValue;
			if (pNextWord[0] == 'm' && pNextWord[1] == 'i' && pNextWord[2] == 'n')
				uiTriggerTimerLength = uiCurrentValue * 60;
		}

		// Case: ... Xs - only support up to 3 digit numbers (in seconds) for this timer parse method
		if (uiCurrentValue == 0)
		{
			char* pCurrentWord = words.Element(i);
			uint32 uiCurrentScanLength = MIN(strlen(pCurrentWord), 4);

			for (uint32 j = 0; j < uiCurrentScanLength; j++)
			{
				if (pCurrentWord[j] >= '0' && pCurrentWord[j] <= '9')
					continue;
				
				if (pCurrentWord[j] == 's')
				{
					pCurrentWord[j] = '\0';
					uiTriggerTimerLength = V_StringToUint32(pCurrentWord, 0, NULL, NULL, PARSING_FLAG_SKIP_WARNING);
				}
				break;
			}
		}
	}
	words.PurgeAndDeleteElements();

	float fCurrentRoundClock = g_pGameRules->m_iRoundTime - (gpGlobals->curtime - g_pGameRules->m_fRoundStartTime.Get().m_Value);

	// Only display trigger time if the timer is greater than 4 seconds, and time expires within the round
	if ((uiTriggerTimerLength > 4) && (fCurrentRoundClock > uiTriggerTimerLength))
	{
		int iTriggerTime = fCurrentRoundClock - uiTriggerTimerLength;

		// Round timer to nearest whole second
		if ((int)(fCurrentRoundClock - 0.5f) == (int)fCurrentRoundClock)
			iTriggerTime++;

		int mins = iTriggerTime / 60;
		int secs = iTriggerTime % 60;

		V_snprintf(buf, sizeof(buf), "%s %s %s %2d:%02d", " \7CONSOLE:\4", pText + sizeof("Console:"), "\x10- @", mins, secs);
	}
	else
		V_snprintf(buf, sizeof(buf), "%s %s", " \7CONSOLE:\4", pText + sizeof("Console:"));

	UTIL_SayTextFilter(filter, buf, pPlayer, eMessageType);
}

// CONVAR_TODO
bool g_bEnableTriggerTimer = false;

CON_COMMAND_F(cs2f_trigger_timer_enable, "Whether to process countdown messages said by Console (e.g. Hold for 10 seconds) and append the round time where the countdown resolves", FCVAR_LINKED_CONCOMMAND | FCVAR_SPONLY)
{
	if (args.ArgC() < 2)
		Msg("%s %i\n", args[0], g_bEnableTriggerTimer);
	else
		g_bEnableTriggerTimer = V_StringToBool(args[1], false);
}

void FASTCALL Detour_UTIL_SayTextFilter(IRecipientFilter &filter, const char *pText, CCSPlayerController *pPlayer, uint64 eMessageType)
{
	if (pPlayer)
		return UTIL_SayTextFilter(filter, pText, pPlayer, eMessageType);

	if (g_bEnableTriggerTimer)
		return SayChatMessageWithTimer(filter, pText, pPlayer, eMessageType);

	char buf[256];
	V_snprintf(buf, sizeof(buf), "%s %s", " \7CONSOLE:\4", pText + sizeof("Console:"));

	UTIL_SayTextFilter(filter, buf, pPlayer, eMessageType);
}

void FASTCALL Detour_UTIL_SayText2Filter(
	IRecipientFilter &filter,
	CCSPlayerController *pEntity,
	uint64 eMessageType,
	const char *msg_name,
	const char *param1,
	const char *param2,
	const char *param3,
	const char *param4)
{
#ifdef _DEBUG
    CPlayerSlot slot = filter.GetRecipientIndex(0);
	CCSPlayerController* target = CCSPlayerController::FromSlot(slot);

	if (target)
		Message("Chat from %s to %s: %s\n", param1, target->GetPlayerName(), param2);
#endif

	UTIL_SayText2Filter(filter, pEntity, eMessageType, msg_name, param1, param2, param3, param4);
}

void Detour_Log()
{
	return;
}

bool FASTCALL Detour_IsChannelEnabled(LoggingChannelID_t channelID, LoggingSeverity_t severity)
{
	return false;
}

CDetour<decltype(Detour_Log)> g_LoggingDetours[] =
{
	CDetour<decltype(Detour_Log)>( Detour_Log, "Msg" ),
	//CDetour<decltype(Detour_Log)>( Detour_Log, "?ConMsg@@YAXPEBDZZ" ),
	//CDetour<decltype(Detour_Log)>( Detour_Log, "?ConColorMsg@@YAXAEBVColor@@PEBDZZ" ),
	CDetour<decltype(Detour_Log)>( Detour_Log, "ConDMsg" ),
	CDetour<decltype(Detour_Log)>( Detour_Log, "DevMsg" ),
	CDetour<decltype(Detour_Log)>( Detour_Log, "Warning" ),
	CDetour<decltype(Detour_Log)>( Detour_Log, "DevWarning" ),
	//CDetour<decltype(Detour_Log)>( Detour_Log, "?DevWarning@@YAXPEBDZZ" ),
	CDetour<decltype(Detour_Log)>( Detour_Log, "LoggingSystem_Log" ),
	CDetour<decltype(Detour_Log)>( Detour_Log, "LoggingSystem_LogDirect" ),
	CDetour<decltype(Detour_Log)>( Detour_Log, "LoggingSystem_LogAssert" ),
	//CDetour<decltype(Detour_Log)>( Detour_IsChannelEnabled, "LoggingSystem_IsChannelEnabled" ),
};

CON_COMMAND_F(toggle_logs, "Toggle printing most logs and warnings", FCVAR_SPONLY | FCVAR_LINKED_CONCOMMAND)
{
	static bool bBlock = false;

	if (!bBlock)
	{
		Message("Logging is now OFF.\n");

		for (int i = 0; i < sizeof(g_LoggingDetours) / sizeof(*g_LoggingDetours); i++)
			g_LoggingDetours[i].EnableDetour();
	}
	else
	{
		Message("Logging is now ON.\n");

		for (int i = 0; i < sizeof(g_LoggingDetours) / sizeof(*g_LoggingDetours); i++)
			g_LoggingDetours[i].DisableDetour();
	}

	bBlock = !bBlock;
}

// CONVAR_TODO
bool g_bBlockNavLookup = false;

CON_COMMAND_F(cs2f_block_nav_lookup, "Whether to block navigation mesh lookup, improves server performance but breaks bot navigation", FCVAR_LINKED_CONCOMMAND | FCVAR_SPONLY)
{
	if (args.ArgC() < 2)
		Msg("%s %i\n", args[0], g_bBlockNavLookup);
	else
		g_bBlockNavLookup = V_StringToBool(args[1], false);
}

void* FASTCALL Detour_CNavMesh_GetNearestNavArea(int64_t unk1, float* unk2, unsigned int* unk3, unsigned int unk4, int64_t unk5, int64_t unk6, float unk7, int64_t unk8)
{
	if (g_bBlockNavLookup)
		return nullptr;

	return CNavMesh_GetNearestNavArea(unk1, unk2, unk3, unk4, unk5, unk6, unk7, unk8);
}

#define CLIPVELOCITY_TIME 0.1f
float g_clipVelocityDisableTime = 0;

void TracePlayerBBox_Custom(const Vector &start, const Vector &end, const bbox_t &bounds, CTraceFilterPlayerMovementCS *filter, trace_t_s2 &pm)
{
	addresses::TracePlayerBBox(start, end, bounds, filter, pm);
	trace_t_s2 test;

	Vector direction = end - start;
	float32 totalDistance = VectorNormalize(direction);

	float32 dotA = direction.Dot(pm.planeNormal);

	// gpGlobals = g_pKZUtils->GetServerGlobals();
	if (pm.fraction < 1 && dotA < 0 && gpGlobals->curtime - g_clipVelocityDisableTime >= CLIPVELOCITY_TIME)
	{
#if 1
		// TODO: this will be 0 if direction is perpendicular to (0,0,1)
		Vector perp1 = CrossProduct(direction, Vector(0, 0, 1));
		Vector perp2 = CrossProduct(direction, perp1);
		VectorNormalize(perp1);
		VectorNormalize(perp2);
		Vector perp3 = -perp1;
		Vector perp4 = -perp2;

		Vector newPos = pm.endpos - (direction * 0.03125f);
		trace_t_s2 tr[4];
		addresses::TracePlayerBBox(newPos, pm.endpos + perp1, bounds, filter, tr[0]);
		addresses::TracePlayerBBox(newPos, pm.endpos + perp2, bounds, filter, tr[1]);
		addresses::TracePlayerBBox(newPos, pm.endpos + perp3, bounds, filter, tr[2]);
		addresses::TracePlayerBBox(newPos, pm.endpos + perp4, bounds, filter, tr[3]);

		int32_t closest = -1;
		int32_t closestFrac = 2.0f;
		for (int32_t i = 0; i < 4; i++)
		{
			if (tr[i].fraction < closestFrac && tr[i].fraction != 1.0)
			{
				closest = i;
				closestFrac = tr[i].fraction;
			}
		}

		if (closest != -1)
		{
			float32 originalFrac = pm.fraction;
			Vector offset = tr[closest].planeNormal * 0.0625f;
			addresses::TracePlayerBBox(
				pm.endpos + offset,
				end + offset, bounds, filter, test
			);
			if (test.fraction != 0)
			{
				pm = test;
				pm.startpos = start;
				// pm.endpos -= offset;
				pm.fraction = originalFrac + (1.0 - originalFrac) * pm.fraction;
			}
		}
#endif
	}
#if 0
	if (pm.fraction < 1 && dotA < -0.25f)
	{
		Vector normal = pm.planeNormal;
		if (dotA > -0.9999f)
		{
			normal = CrossProduct(pm.planeNormal, direction);
			VectorNormalize(normal);
		}
		else
		{
			// for breakpoint
			normal = pm.planeNormal;
		}
		Vector pos1 = pm.endpos + (normal * 0.03125f);
		Vector pos2 = pm.endpos - (normal * 0.03125f);
		utils::TracePlayerBBox(pos1, pos2, bounds, filter, test);
		if (test.startsolid)
		{
			utils::TracePlayerBBox(pos2, pos1, bounds, filter, test);
		}
		if (test.fraction < 1 && !test.startsolid)
		{
			f32 dot = direction.Dot(test.planeNormal);
			if (dot >= -0.25f)
			{
				// Msg("Success! a %f b %f\n", dotA, dot);
				pm.planeNormal = test.planeNormal;
			}
			else
			{
				// Msg("Fail :(! a %f b %f\n", dotA, dot);
			}
		}
	}
#endif

	// attempt to travel the rest of the distance if we're stuck somewhere or stuck on an edge
	for (int32_t i = 0; i < 4; i++)
	{
		addresses::TracePlayerBBox(pm.endpos, pm.endpos, bounds, filter, test);
		if (pm.fraction != 1.0 && test.startsolid)
		{
			addresses::TracePlayerBBox(pm.endpos + (test.planeNormal * 0.03125f), end, bounds, filter, test);
			if (test.fraction)
			{
				pm.endpos = test.endpos;
				if (totalDistance > 0.0)
				{
					pm.fraction = VectorLength(test.endpos - start) / totalDistance;
					pm.endpos = test.endpos;
					// pm.planeNormal = test.planeNormal;
				}
			}
			else
			{
				break;
			}
		}
	}
}


void ClipVelocity_Custom(Vector &in, Vector &normal, Vector &out, float32 overbounce)
{
	// Determine how far along plane to slide based on incoming direction.
	float32 backoff = DotProduct(in, normal) * overbounce;

	for (int32_t i = 0; i < 3; i++)
	{
		float32 change = normal[i] * backoff;
		out[i] = in[i] - change;
	}

	// Rampbug/wallbug fix: always move a little bit away from the plane
	float adjust = -0.5f;
	out -= (normal * adjust);
}

#define MAX_CLIP_PLANES 4
#define GM_MV_OPTIMISATIONS 1
#if 0
void TryPlayerMove_Custom(CCSPlayer_MovementServices *ms, CMoveData *mv, Vector *pFirstDest, trace_t_s2 *pFirstTrace)
{
	int			bumpcount, numbumps;
	Vector		dir;
	float		d;
	int			numplanes;
	Vector		planes[MAX_CLIP_PLANES];
	Vector		primal_velocity, original_velocity, original_original_velocity;
	Vector      new_velocity;
	int			i, j;
	trace_t_s2	pm;
	Vector		end;
	float		time_left, allFraction;
	int			blocked;

	numbumps  = 4;           // Bump up to four times

	blocked   = 0;           // Assume not blocked
	numplanes = 0;           //  and not sliding along any planes

	VectorCopy (mv->m_vecVelocity, original_original_velocity);  // Store original velocity
	VectorCopy (mv->m_vecVelocity, original_velocity);  // Store original velocity
	VectorCopy (mv->m_vecVelocity, primal_velocity);

	allFraction = 0;
	time_left = gpGlobals->frametime;   // Total time for this movement operation.


	new_velocity.Init();

	// return this->players[ms->pawn->m_hController().GetEntryIndex()];
	ZEPlayer *zePlayer = g_playerManager->GetPlayer(ms->pawn->m_hController()->GetPlayerSlot());
	CBasePlayerPawn *pawn = ms->pawn;
	CCSPlayerPawn *player = dynamic_cast<CCSPlayerPawn *>(pawn);
	
	// zePlayer->GetSteamId64()
	// Message("TryPlayerMove_steamid: %llu\n", zePlayer->GetSteamId64());
	
	CTraceFilterPlayerMovementCS filter;
	addresses::InitPlayerMovementTraceFilter(&filter, player, player->m_Collision().m_collisionAttribute().m_nInteractsWith(), COLLISION_GROUP_PLAYER_MOVEMENT);

	// Debug print filter
	// Message("filter.m_nInteractsWith: %llu\n", filter.m_nInteractsWith);
	// Message("filter.m_nInteractsExclude: %llu\n", filter.m_nInteractsExclude);
	// Message("filter.m_nInteractsAs: %llu\n", filter.m_nInteractsAs);
	// Message("filter.m_nEntityId[0]: %u\n", filter.m_nEntityId[0]);
	// Message("filter.m_nEntityId[1]: %u\n", filter.m_nEntityId[1]);
	// Message("filter.m_nOwnerId[0]: %u\n", filter.m_nOwnerId[0]);
	// Message("filter.m_nOwnerId[1]: %u\n", filter.m_nOwnerId[1]);
	// Message("filter.m_nHierarchyId[0]: %u\n", filter.m_nHierarchyId[0]);
	// Message("filter.m_nHierarchyId[1]: %u\n", filter.m_nHierarchyId[1]);
	// Message("filter.m_nCollisionFunctionMask: %u\n", filter.m_nCollisionFunctionMask);
	// Message("filter.unk2: %u\n", filter.unk2);
	// Message("filter.m_nCollisionGroup: %u\n", filter.m_nCollisionGroup);
	// Message("filter.unk3: %u\n", filter.unk3);
	// Message("filter.unk4: %u\n", filter.unk4);

	bbox_t bounds;
	bounds.mins = { -16, -16, 0 };
	bounds.maxs = { 16, 16, 72 };

	if (ms->m_bDucked())
	{
		bounds.maxs.z = 54;
	}

	for (bumpcount=0 ; bumpcount < numbumps; bumpcount++)
	{
		if ( mv->m_vecVelocity.Length() == 0.0 )
			break;

		// Assume we can move all the way from the current origin to the
		//  end point.
		VectorMA( mv->m_vecAbsOrigin, time_left, mv->m_vecVelocity, end );

		// See if we can make it from origin to end point.
		if ( GM_MV_OPTIMISATIONS )
		{
			// If their velocity Z is 0, then we can avoid an extra trace here during WalkMove.
			if ( pFirstDest && end == *pFirstDest )
				pm = *pFirstTrace;
			else
			{
				TracePlayerBBox_Custom( mv->m_vecAbsOrigin, end, bounds, &filter, pm );
			}
		}
		else
		{
			TracePlayerBBox_Custom( mv->m_vecAbsOrigin, end, bounds, &filter, pm );
		}

		allFraction += pm.fraction;

		// If we started in a solid object, or we were in solid space
		//  the whole way, zero out our velocity and return that we
		//  are blocked by floor and wall.
		trace_t_s2 stuckTest;
		TracePlayerBBox_Custom( end, mv->m_vecAbsOrigin, bounds, &filter, stuckTest );
		if (pm.startsolid && pm.fraction == 0 && stuckTest.startsolid && stuckTest.fraction == 0)
		{
			// entity is trapped in another solid
			// VectorCopy (vec3_origin, mv->m_vecVelocity);
			// return;
		}

		// If we moved some portion of the total distance, then
		//  copy the end position into the pmove.origin and 
		//  zero the plane counter.
		if( pm.fraction > 0 )
		{
			if ( numbumps > 0 && pm.fraction == 1 )
			{
				// There's a precision issue with terrain tracing that can cause a swept box to successfully trace
				// when the end position is stuck in the triangle.  Re-run the test with an uswept box to catch that
				// case until the bug is fixed.
				// If we detect getting stuck, don't allow the movement
				trace_t_s2 stuck;
				TracePlayerBBox_Custom( pm.endpos, pm.endpos, bounds, &filter, stuck );
				if ( stuck.startsolid || stuck.fraction != 1.0f )
				{
					//Msg( "Player will become stuck!!!\n" );
					VectorCopy (vec3_origin, mv->m_vecVelocity);
					break;
				}
			}

			// actually covered some distance
			mv->m_vecAbsOrigin = pm.endpos;
			VectorCopy (mv->m_vecVelocity, original_velocity);
			numplanes = 0;
		}

		// If we covered the entire distance, we are done
		//  and can return.
		if (pm.fraction == 1)
		{
			break;		// moved the entire distance
		}
		
		// Save entity that blocked us (since fraction was < 1.0)
		//  for contact
		// Add it if it's not already in the list!!!
		// TODO:?
		//MoveHelper( )->AddToTouched( pm, mv->m_vecVelocity );

		// If the plane we hit has a high z component in the normal, then
		//  it's probably a floor
		if (pm.planeNormal[2] > 0.7)
		{
			blocked |= 1;		// floor
		}
		// If the plane has a zero z component in the normal, then it's a 
		//  step or wall
		if (!pm.planeNormal[2])
		{
			blocked |= 2;		// step / wall
		}

		// Reduce amount of m_flFrameTime left by total time left * fraction
		//  that we covered.
		time_left -= time_left * pm.fraction;

		// Did we run out of planes to clip against?
		if (numplanes >= MAX_CLIP_PLANES)
		{	
			// this shouldn't really happen
			//  Stop our movement if so.
			VectorCopy (vec3_origin, mv->m_vecVelocity);
			//Con_DPrintf("Too many planes 4\n");

			break;
		}

		// Set up next clipping plane
		VectorCopy (pm.planeNormal, planes[numplanes]);
		numplanes++;

		// modify original_velocity so it parallels all of the clip planes
		//

		// reflect player velocity 
		// Only give this a try for first impact plane because you can get yourself stuck in an acute corner by jumping in place
		//  and pressing forward and nobody was really using this bounce/reflection feature anyway...
		if ( numplanes == 1 &&
			player->m_MoveType() == MOVETYPE_WALK &&
			player->m_hGroundEntity() == NULL )	
		{
			for ( i = 0; i < numplanes; i++ )
			{
				if ( planes[i][2] > 0.7  )
				{
					// floor or slope
					ClipVelocity_Custom( original_velocity, planes[i], new_velocity, 1 );
					VectorCopy( new_velocity, original_velocity );
				}
				else
				{
					// TODO: m_flSurfaceFriction and maybe sv_bounce
					ClipVelocity_Custom( original_velocity, planes[i], new_velocity, 1.0);
				}
			}

			VectorCopy( new_velocity, mv->m_vecVelocity );
			VectorCopy( new_velocity, original_velocity );
		}
		else
		{
			for (i=0 ; i < numplanes ; i++)
			{
				ClipVelocity_Custom (
							  original_velocity,
							  planes[i],
							  mv->m_vecVelocity,
							  1);

				for (j=0 ; j<numplanes ; j++)
					if (j != i)
					{
						// Are we now moving against this plane?
						if (mv->m_vecVelocity.Dot(planes[j]) < 0)
							break;	// not ok
					}
				if (j == numplanes)  // Didn't have to clip, so we're ok
					break;
			}

			// Did we go all the way through plane set
			if (i != numplanes)
			{	// go along this plane
				// pmove.velocity is set in clipping call, no need to set again.
				;  
			}
			else
			{	// go along the crease
				if (numplanes != 2)
				{
					VectorCopy (vec3_origin, mv->m_vecVelocity);
					break;
				}
				CrossProduct (planes[0], planes[1], dir);
				dir.NormalizeInPlace();
				d = dir.Dot(mv->m_vecVelocity);
				VectorScale (dir, d, mv->m_vecVelocity );
			}

			//
			// if original velocity is against the original velocity, stop dead
			// to avoid tiny occilations in sloping corners
			//
			d = mv->m_vecVelocity.Dot(primal_velocity);
			if (d <= 0)
			{
				//Con_DPrintf("Back\n");
				VectorCopy (vec3_origin, mv->m_vecVelocity);
				break;
			}
		}
	}
	if ( allFraction == 0 )
	{
		VectorCopy (vec3_origin, mv->m_vecVelocity);
	}

	if (gpGlobals->curtime - g_clipVelocityDisableTime < CLIPVELOCITY_TIME)
	{
		VectorCopy(original_original_velocity, mv->m_vecVelocity);
	}

#if 0
	// Check if they slammed into a wall
	float fSlamVol = 0.0f;

	float fLateralStoppingAmount = primal_velocity.Length2D() - mv->m_vecVelocity.Length2D();
	if ( fLateralStoppingAmount > PLAYER_MAX_SAFE_FALL_SPEED * 2.0f )
	{
		fSlamVol = 1.0f;
	}
	else if ( fLateralStoppingAmount > PLAYER_MAX_SAFE_FALL_SPEED )
	{
		fSlamVol = 0.85f;
	}

	PlayerRoughLandingEffects( fSlamVol );

	return blocked;
#endif
}
#else
bool IsValidMovementTrace(trace_t_s2 &tr, bbox_t bounds, CTraceFilterPlayerMovementCS *filter)
{
	trace_t_s2 stuck;

	// Apparently we can be stuck with pm.allsolid without having valid plane info ok..
	// TODO: allsolid
	// if (tr.allsolid || tr.startsolid)
	if (tr.startsolid && tr.fraction == 0)
	{
		return false;
	}

	// Maybe we don't need this one
	if (CloseEnough(tr.fraction, 0.0f, FLT_EPSILON))
	{
		return false;
	}

	if (CloseEnough(tr.fraction, 0.0f, FLT_EPSILON) &&
		CloseEnough(tr.planeNormal, Vector(0.0f, 0.0f, 0.0f), FLT_EPSILON))
	{
		return false;
	}

	// Is the plane deformed or some stupid shit?
	if (fabs(tr.planeNormal.x) > 1.0f || fabs(tr.planeNormal.y) > 1.0f || fabs(tr.planeNormal.z) > 1.0f)
	{
		return false;
	}

	addresses::TracePlayerBBox(tr.endpos, tr.endpos, bounds, filter, stuck);
	if (stuck.startsolid || !CloseEnough(stuck.fraction, 1.0f, FLT_EPSILON))
	{
		return false;
	}

	return true;
}

void TryPlayerMove_Custom(CCSPlayer_MovementServices *ms, CMoveData *mv, Vector *pFirstDest, trace_t_s2 *pFirstTrace)
{
	int bumpcount, numbumps;
	Vector dir;
	float d;
	int numplanes;
	Vector planes[MAX_CLIP_PLANES];
	Vector primal_velocity, original_velocity;
	Vector new_velocity;
	Vector fixed_origin;
	Vector valid_plane;
	int i, j, h;
	trace_t_s2 pm;
	Vector end;
	float time_left, allFraction;
	int blocked;
	bool stuck_on_ramp;
	bool has_valid_plane;
	numbumps = 8; // sv_ramp_bumpcount.GetInt();

	blocked = 0;   // Assume not blocked
	numplanes = 0; //  and not sliding along any planes

	stuck_on_ramp = false;   // lets assume client isn't stuck already
	has_valid_plane = false; // no plane info gathered yet

	VectorCopy(mv->m_vecVelocity, original_velocity); // Store original velocity
	VectorCopy(mv->m_vecVelocity, primal_velocity);
	VectorCopy(mv->m_vecAbsOrigin, fixed_origin);

	// return this->players[ms->pawn->m_hController().GetEntryIndex()];
	ZEPlayer *zePlayer = g_playerManager->GetPlayer(ms->pawn->m_hController()->GetPlayerSlot());
	CBasePlayerPawn *pawn = ms->pawn;
	CCSPlayerPawn *player = dynamic_cast<CCSPlayerPawn *>(pawn);
	
	// zePlayer->GetSteamId64()
	// Message("TryPlayerMove_steamid: %llu\n", zePlayer->GetSteamId64());
	
	CTraceFilterPlayerMovementCS filter;
	addresses::InitPlayerMovementTraceFilter(&filter, player, player->m_Collision().m_collisionAttribute().m_nInteractsWith(), COLLISION_GROUP_PLAYER_MOVEMENT);

	
	bbox_t bounds;
	bounds.mins = { -16, -16, 0 };
	bounds.maxs = { 16, 16, 72 };

	allFraction = 0;
	time_left = gpGlobals->frametime; // Total time for this movement operation.

	new_velocity.Init();
	valid_plane.Init();

	Vector vecWallNormal;
	bool   bWallNormSet = false;

	float sv_ramp_initial_retrace_length = 0.2f;

	for (bumpcount = 0; bumpcount < numbumps; bumpcount++)
	{
		if (mv->m_vecVelocity.Length() == 0.0)
			break;

		if (stuck_on_ramp)
		{
			if (!has_valid_plane)
			{
				if (!CloseEnough(pm.planeNormal, Vector(0.0f, 0.0f, 0.0f), FLT_EPSILON) &&
					valid_plane != pm.planeNormal)
				{
					valid_plane = pm.planeNormal;
					has_valid_plane = true;
				}
				else
				{
					for (i = numplanes; i-- > 0;)
					{
						if (!CloseEnough(planes[i], Vector(0.0f, 0.0f, 0.0f), FLT_EPSILON) &&
							fabs(planes[i].x) <= 1.0f && fabs(planes[i].y) <= 1.0f && fabs(planes[i].z) <= 1.0f &&
							valid_plane != planes[i])
						{
							valid_plane = planes[i];
							has_valid_plane = true;
							break;
						}
					}
				}
			}

			if (has_valid_plane)
			{
				if (valid_plane.z >= 0.7f && valid_plane.z <= 1.0f)
				{
					ClipVelocity_Custom(mv->m_vecVelocity, valid_plane, mv->m_vecVelocity, 1);
					VectorCopy(mv->m_vecVelocity, original_velocity);
				}
				else
				{
					ClipVelocity_Custom(mv->m_vecVelocity, valid_plane, mv->m_vecVelocity, 1.0f);
					VectorCopy(mv->m_vecVelocity, original_velocity);
				}
			}
			else // We were actually going to be stuck, lets try and find a valid plane..
			{
				// this way we know fixed_origin isn't going to be stuck
				float offsets[] = {(bumpcount * 2) * -sv_ramp_initial_retrace_length, 0.0f,
								   (bumpcount * 2) * sv_ramp_initial_retrace_length};
				int valid_planes = 0;
				valid_plane.Init(0.0f, 0.0f, 0.0f);

				// we have 0 plane info, so lets increase our bbox and search in all 27 directions to get a valid plane!
				for (i = 0; i < 3; i++)
				{
					for (j = 0; j < 3; j++)
					{
						for (h = 0; h < 3; h++)
						{
							Vector offset = {offsets[i], offsets[j], offsets[h]};

							Vector offset_mins = offset / 2.0f;
							Vector offset_maxs = offset / 2.0f;

							if (offset.x > 0.0f)
								offset_mins.x /= 2.0f;
							if (offset.y > 0.0f)
								offset_mins.y /= 2.0f;
							if (offset.z > 0.0f)
								offset_mins.z /= 2.0f;

							if (offset.x < 0.0f)
								offset_maxs.x /= 2.0f;
							if (offset.y < 0.0f)
								offset_maxs.y /= 2.0f;
							if (offset.z < 0.0f)
								offset_maxs.z /= 2.0f;
#if 0
							Ray_t ray;
							ray.Init(fixed_origin + offset, end - offset, GetPlayerMins(ms) - offset_mins,
									 GetPlayerMaxs(ms) + offset_maxs);
							UTIL_TraceRay(ray, PlayerSolidMask(), mv->m_nPlayerHandle.Get(),
										  COLLISION_GROUP_PLAYER_MOVEMENT, &pm);
#endif
							bbox_t bounds2 = bounds;
							bounds.mins -= offset_mins;
							bounds.maxs -= offset_maxs;
							addresses::TracePlayerBBox(fixed_origin + offset, end - offset, bounds2, &filter, pm);

							// Only use non deformed planes and planes with values where the start point is not from a
							// solid
							if (fabs(pm.planeNormal.x) <= 1.0f && fabs(pm.planeNormal.y) <= 1.0f &&
								fabs(pm.planeNormal.z) <= 1.0f && pm.fraction > 0.0f && pm.fraction < 1.0f &&
								!pm.startsolid)
							{
								valid_planes++;
								valid_plane += pm.planeNormal;
							}
						}
					}
				}

				if (valid_planes && !CloseEnough(valid_plane, Vector(0.0f, 0.0f, 0.0f), FLT_EPSILON))
				{
					has_valid_plane = true;
					valid_plane.NormalizeInPlace();
					continue;
				}
			}

			if (has_valid_plane)
			{
				VectorMA(fixed_origin, sv_ramp_initial_retrace_length, valid_plane, fixed_origin);
			}
			else
			{
				stuck_on_ramp = false;
				continue;
			}
		}

		// Assume we can move all the way from the current origin to the
		//  end point.

		VectorMA(fixed_origin, time_left, mv->m_vecVelocity, end);

		// See if we can make it from origin to end point.
		// If their velocity Z is 0, then we can avoid an extra trace here during WalkMove.
		if (pFirstDest && end == *pFirstDest)
			pm = *pFirstTrace;
		else
		{
#if defined(PLAYER_GETTING_STUCK_TESTING)
			trace_t foo;
			TracePlayerBBox(mv->m_vecAbsOrigin, mv->m_vecAbsOrigin, PlayerSolidMask(), COLLISION_GROUP_PLAYER_MOVEMENT,
							foo);
			if (foo.startsolid || foo.fraction != 1.0f)
			{
				Msg("bah\n");
			}
#endif
			if (stuck_on_ramp && has_valid_plane)
			{
				addresses::TracePlayerBBox(fixed_origin, end, bounds, &filter, pm);
				pm.planeNormal = valid_plane;
			}
			else
			{
				addresses::TracePlayerBBox(mv->m_vecAbsOrigin, end, bounds, &filter, pm);
				// Check if the collision happens at the corner of the collision box.
				if (pm.fraction != 1.0f && IsValidMovementTrace(pm, bounds, &filter)
					&& fabs(pm.planeNormal.x) < 0.95f && fabs(pm.planeNormal.y) < 0.95f && fabs(pm.planeNormal.z) < 0.95f)
				{
					uint32_t retestType = (int)(fabs(pm.planeNormal.x) > 0.001f) + (int)(fabs(pm.planeNormal.y) > 0.001f) + (int)(fabs(pm.planeNormal.z) > 0.001f);
					switch (retestType)
					{
						case 1: // No plane can have a value of 1, so something went really wrong.
						{
							//Warning("Bogus plane collision detected!");
							break;
						}
						case 2: // Collision on a player's bbox line.
						{
							bool badDestination = false;
							uint32_t lineDirection = 2;
							float32 retraceFraction = 0.0f;
							Vector offset, extendedOffset;
							for (uint32_t i = 0; i < 3; i++)
							{
								if (CloseEnough(pm.planeNormal[i], 0.0f, FLT_EPSILON))
								{
									lineDirection = i;
									continue;
								}
								offset[i] = pm.planeNormal[i] < 0.0f ? bounds.maxs[i] - 0.03125f : bounds.mins[i] + 0.03125f;
								extendedOffset[i] = pm.planeNormal[i] < 0.0f ? bounds.maxs[i] + 0.03125f : bounds.mins[i] - 0.03125f;
							}
							// First, trace along the line and see if we actually did hit something.
							bbox_t lineBounds;
							lineBounds.mins[lineDirection] = bounds.mins[lineDirection];
							lineBounds.maxs[lineDirection] = bounds.maxs[lineDirection];

							trace_t_s2 linepm;
							addresses::TracePlayerBBox(mv->m_vecAbsOrigin + offset, end + offset, lineBounds, &filter, linepm);

							retraceFraction = linepm.fraction;
							if (retraceFraction > pm.fraction)
							{
								// Trace back from the end to the collision spot just to be sure we did not hit anything else.
								trace_t_s2 verify;
								if (CloseEnough(retraceFraction, 1.0f, FLT_EPSILON))
								{
									addresses::TracePlayerBBox(end, pm.endpos, bounds, &filter, verify);
								}
								else
								{
									addresses::TracePlayerBBox(linepm.endpos - extendedOffset, pm.endpos, bounds, &filter, verify);
								}
								// The end positions are so close they might as well be the same spot.
								bool sameSpot = (pm.endpos - verify.endpos).Length() < 0.2f;
								if ((player->m_hGroundEntity() == nullptr && !IsValidMovementTrace(verify, bounds, &filter)))
								{
									// Somehow we get stuck at the destination. This trace isn't actually real either.
									badDestination = true;
								}
								else if (sameSpot)
								{
									Vector startPos = pm.startpos;
									pm = linepm;
									pm.startpos = startPos;
									pm.endpos = linepm.endpos - offset;
									pm.traceType = 2;
									pm.fraction = retraceFraction;
									break;
								}
							}
							// Second, trace from the two corners of the line.
							// It should be extremely rare that a rampbug happens without either corner touching a ramp. It is most likely not a ramp bug in that case.
							Vector offsets[2], extendedOffsets[2];
							for (uint32_t i = 0; i < 2; i++)
							{
								for (uint32_t j = 0; j < 3; j++)
								{
									if (j == lineDirection)
									{
										continue;
									}
									offsets[i][j] = pm.planeNormal[i] < 0.0f ? bounds.maxs[i] - 0.03125f : bounds.mins[i] + 0.03125f;
									extendedOffsets[i][j] = pm.planeNormal[i] < 0.0f ? bounds.maxs[i] + 0.03125f : bounds.mins[i] - 0.03125f;
								}
							}
							offsets[0][lineDirection] = bounds.mins[lineDirection] + 0.03125f;
							extendedOffsets[0][lineDirection] = bounds.mins[lineDirection] - 0.03125f;
							offsets[1][lineDirection] = bounds.maxs[lineDirection] + 0.03125f;
							extendedOffsets[1][lineDirection] = bounds.maxs[lineDirection] - 0.03125f;
							bbox_t emptyBounds = { Vector(0,0,0), Vector(0,0,0) };
							trace_t_s2 cornerpm[2];
							for (uint32_t i = 0; i < 2; i++)
							{
								addresses::TracePlayerBBox(mv->m_vecAbsOrigin + offsets[i], end + offsets[i], emptyBounds, &filter, cornerpm[i]);
							}
							// Take the shorter trace as our hopefully "real" trace.
							uint32_t realTrace = cornerpm[0].fraction < cornerpm[1].fraction ? 0 : 1;
							retraceFraction = cornerpm[realTrace].fraction;
							if (retraceFraction > pm.fraction)
							{
								// Trace back from the end to the collision spot just to be sure we did not hit anything else.
								trace_t_s2 verify;
								if (CloseEnough(retraceFraction, 1.0f, FLT_EPSILON))
								{
									addresses::TracePlayerBBox(end, pm.endpos, bounds, &filter, verify);
								}
								else
								{
									addresses::TracePlayerBBox(cornerpm[realTrace].endpos - extendedOffsets[realTrace], pm.endpos, bounds, &filter, verify);
								}
								// The end positions are so close they might as well be the same spot.
								bool sameSpot = (pm.endpos - verify.endpos).Length() < 0.2f;
								if ((player->m_hGroundEntity() == nullptr && !IsValidMovementTrace(verify, bounds, &filter)))
								{
									badDestination = true;
								}
								if (sameSpot)
								{
									Vector startPos = pm.startpos;
									pm = cornerpm[realTrace];
									pm.startpos = startPos;
									pm.endpos = cornerpm[realTrace].endpos - offsets[realTrace];
									pm.traceType = 2;
									pm.fraction = retraceFraction;
								}
							}
							if (badDestination)
							{
								// Somehow we get stuck at the destination. None of the traces are usable.
								has_valid_plane = false;
								stuck_on_ramp = true;
								continue;
							}
						}
						case 3: // Collision on player's corners.
						{
							Vector offset, extendedOffset;
							for (uint32_t i = 0; i < 3; i++)
							{
								offset[i] = pm.planeNormal[i] < 0.0f ? bounds.maxs[i] - 0.03125f : bounds.mins[i] + 0.03125f;
								extendedOffset[i] = pm.planeNormal[i] < 0.0f ? bounds.maxs[i] + 0.03125f : bounds.mins[i] - 0.03125f;
							}
							bbox_t emptyBounds = { Vector(0,0,0), Vector(0,0,0) };
							trace_t_s2 cornerpm;
							addresses::TracePlayerBBox(mv->m_vecAbsOrigin + offset, end + offset, emptyBounds, &filter, cornerpm);
							//utils::DebugLine(mv->m_vecAbsOrigin + offset, end + offset, 0, 255, 255, true, 3.0f);
							float32 retraceFraction = cornerpm.fraction;
							if (retraceFraction > pm.fraction)
							{
								// Trace back from the end to the collision spot just to be sure we did not hit anything else.
								trace_t_s2 verify;
								if (CloseEnough(retraceFraction, 1.0f, FLT_EPSILON))
								{
									addresses::TracePlayerBBox(end, pm.endpos, bounds, &filter, verify);
									// utils::DebugLine(end, pm.endpos, 255, 255, 0, true, 3.0f);
								}
								else
								{
									addresses::TracePlayerBBox(cornerpm.endpos - extendedOffset, pm.endpos, bounds, &filter, verify);
									// utils::DebugLine(cornerpm.endpos - extendedOffset, pm.endpos, 255, 255, 128, true, 3.0f);
								}
								// The end positions are so close they might as well be the same spot.
								bool sameSpot = (pm.endpos - verify.endpos).Length() < 0.2f;
								if ((player->m_hGroundEntity() == nullptr && !IsValidMovementTrace(verify, bounds, &filter)))
								{
									// Somehow we get stuck at the destination. This trace isn't actually real either.
									has_valid_plane = false;
									stuck_on_ramp = true;
									continue;
								}
								if (sameSpot)
								{
									Vector startPos = pm.startpos;
									pm = cornerpm;
									pm.startpos = startPos;
									pm.endpos = cornerpm.endpos - offset;
									pm.traceType = 2;
									pm.fraction = retraceFraction;
								}
							}
						}
					}
				}
				//utils::DebugLine(pm.endpos, pm.endpos + 5 * pm.planeNormal, 255, 0, 0, true, 3.0f);
				//utils::DebugCross3D(pm.endpos, 16, 255, 0, 0, true, 3.0f);
				if (player->m_hGroundEntity() == nullptr && !IsValidMovementTrace(pm, bounds, &filter))
				{
					has_valid_plane = false;
					stuck_on_ramp = true;
					// Fully stuck trace can somehow have startstuck at false, and a plane normal. Revert this nonsense.
					if (CloseEnough(pm.fraction, 0.0f, FLT_EPSILON))
					{
						VectorCopy(vec3_origin, pm.planeNormal);
					}
					continue;
				}
				// We aren't stuck yet, so what if we just ignore whatever we hit just now and keep moving?
				if (!CloseEnough(pm.fraction, 1.0f, FLT_EPSILON))
				{
					bumpcount++;
					trace_t_s2 pm2;
					addresses::TracePlayerBBox(pm.endpos, end, bounds, &filter, pm2);
					//utils::DebugLine(pm2.endpos, pm2.endpos + 8 * pm.planeNormal, 0, 255, 0, true, 10.0f);
					//utils::DebugCross3D(pm2.endpos, 5, 255, 0, 0, true, 3.0f);
					if (player->m_hGroundEntity() == nullptr && CloseEnough(pm2.fraction, 0.0f, FLT_EPSILON))
					{
						trace_t_s2 stuck;
						addresses::TracePlayerBBox(pm2.endpos, pm2.endpos, bounds, &filter, stuck);
						if (pm2.startsolid || stuck.startsolid || !CloseEnough(stuck.fraction, 1.0f, FLT_EPSILON))
						{
							has_valid_plane = false;
							stuck_on_ramp = true;
							if (CloseEnough(pm.fraction, 0.0f, FLT_EPSILON))
							{
								VectorCopy(vec3_origin, pm.planeNormal);
							}
							continue;
						}
					}
					if (CloseEnough(pm2.fraction, 1.0f, FLT_EPSILON))
					{
						Vector startPos = pm.startpos;
						pm = pm2;
						pm.startpos = startPos;
						break;
					}
					//// We did hit something, but what if we move the original origin just a tiny bit away from the normal of whatever we just collided?
					//else if (!CloseEnough(pm2.fraction, 0.0f, 0.1f))
					//{
					//	Vector newStartPos = mv->m_vecAbsOrigin + pm2.planeNormal * 0.0625f;
					//	end += pm2.planeNormal * 0.0625f;
					//	utils::TracePlayerBBox(newStartPos, end, bounds, &filter, pm2);
					//	utils::DebugLine(pm2.endpos, pm2.endpos + 8 * pm.planeNormal, 0, 0, 255, true, 10.0f);
					//	utils::DebugCross3D(pm2.endpos, 5, 255, 0, 0, true, 3.0f);
					//	if (player->m_hGroundEntity() == nullptr && !IsValidMovementTrace(pm2, bounds, &filter))
					//	{
					//		has_valid_plane = false;
					//		stuck_on_ramp = true;
					//		continue;
					//	}
					//	if (IsValidMovementTrace(pm2, bounds, &filter) && (pm2.planeNormal != pm.planeNormal || CloseEnough(pm2.fraction, 1.0f, FLT_EPSILON)))
					//	{
					//		Vector startPos = pm.startpos;
					//		pm = pm2;
					//		pm.startpos = startPos;
					//		if (CloseEnough(pm2.fraction, 1.0f, FLT_EPSILON))
					//		{
					//			break;
					//		}
					//	}
					//}
				}
#if 0
				if (player->m_MoveType() == MOVETYPE_WALK &&
					player->m_hGroundEntity() == nullptr //&& player->GetWaterLevel() < WL_Waist &&
					)
				{
					// bool bValidHit = !pm.allsolid && pm.fraction < 1.0f;
					bool bValidHit = !pm.startsolid && pm.fraction != 0 && pm.fraction < 1.0f;

					bool bCouldStandHere = pm.planeNormal.z >= 0.7f && 
										   mv->m_vecVelocity.z <= 140.0f;

					bool bMovingIntoPlane2D = (pm.planeNormal.x * mv->m_vecVelocity.x) + (pm.planeNormal.y * mv->m_vecVelocity.y) < 0.0f;

					// Don't perform this fix on additional collisions this tick which have trace fraction == 0.0.
					// This situation occurs when wedged between a standable slope and a ceiling.
					// The player would be locked in place with full velocity (but no movement) without this check.
					bool bWedged = m_pPlayer->GetInteraction(0).tick == gpGlobals->tickcount && pm.fraction == 0.0f;

					if (bValidHit && bCouldStandHere && bMovingIntoPlane2D && !bWedged)
					{
						Vector vecNewVelocity;
						ClipVelocity_Custom(mv->m_vecVelocity, pm.planeNormal, vecNewVelocity, 1.0f);

						// Make sure allowing this collision would not actually be beneficial (2D speed gain)
						if (vecNewVelocity.Length2DSqr() <= mv->m_vecVelocity.Length2DSqr())
						{
							// A fraction of 1.0 implies no collision, which means ClipVelocity will not be called.
							// It also suggests movement for this tick is complete, so TryPlayerMove won't perform
							// additional movement traces and the tick will essentially end early. We want this to
							// happen because we need landing/jumping logic to be applied before movement continues.
							// Ending the tick early is a safe and easy way to do this.

							pm.fraction = 1.0f;
						}
					}
				}
#endif
			}
		}

		if (bumpcount && player->m_hGroundEntity() == nullptr && !IsValidMovementTrace(pm, bounds, &filter))
		{
			has_valid_plane = false;
			stuck_on_ramp = true;
			continue;
		}

		// If we moved some portion of the total distance, then
		//  copy the end position into the pmove.origin and
		//  zero the plane counter.
		if (pm.fraction > 0.0f)
		{
			if (!bumpcount || player->m_hGroundEntity() != nullptr)
			{
				// There's a precision issue with terrain tracing that can cause a swept box to successfully trace
				// when the end position is stuck in the triangle.  Re-run the test with an unswept box to catch that
				// case until the bug is fixed.
				// If we detect getting stuck, don't allow the movement
				trace_t_s2 stuck;
				addresses::TracePlayerBBox(pm.endpos, pm.endpos, bounds, &filter, stuck);

				if ((stuck.startsolid || stuck.fraction != 1.0f) && !bumpcount)
				{
					has_valid_plane = false;
					stuck_on_ramp = true;
					continue;
				}
				else if (stuck.startsolid || stuck.fraction != 1.0f)
				{
#if 0
					Msg("Player will become stuck!!! allfrac: %f pm: %i, %f, %f, %f vs stuck: %i, %f, %f\n",
						allFraction, pm.startsolid, pm.fraction, pm.planeNormal.z, pm.fractionleftsolid,
						stuck.startsolid, stuck.fraction, stuck.planeNormal.z);
#endif
					VectorCopy(vec3_origin, mv->m_vecVelocity);
					break;
				}
			}

#if defined(PLAYER_GETTING_STUCK_TESTING)
			trace_t foo;
			TracePlayerBBox(pm.endpos, pm.endpos, PlayerSolidMask(), COLLISION_GROUP_PLAYER_MOVEMENT, foo);
			if (foo.startsolid || foo.fraction != 1.0f)
			{
				Msg("Player will become stuck!!!\n");
			}
#endif
			{
				has_valid_plane = false;
				stuck_on_ramp = false;
			}

			// actually covered some distance
			VectorCopy(mv->m_vecVelocity, original_velocity);
			mv->m_vecAbsOrigin = pm.endpos;
			VectorCopy(mv->m_vecAbsOrigin, fixed_origin);
			allFraction += pm.fraction;
			numplanes = 0;
		}

		// If we covered the entire distance, we are done
		//  and can return.
		if (CloseEnough(pm.fraction, 1.0f, FLT_EPSILON))
		{
			break; // moved the entire distance
		}

		// Save entity that blocked us (since fraction was < 1.0)
		//  for contact
		// Add it if it's not already in the list!!!
		// MoveHelper()->AddToTouched(pm, mv->m_vecVelocity);

		// If the plane we hit has a high z component in the normal, then
		//  it's probably a floor
		if (pm.planeNormal[2] >= 0.7)
		{
			blocked |= 1; // floor
		}

		if (CloseEnough(pm.planeNormal[2], 0.0f, FLT_EPSILON))
		{
			// If the plane has a zero z component in the normal, then it's a step or wall
			blocked |= 2; // step / wall
		}

		// Reduce amount of m_flFrameTime left by total time left * fraction
		//  that we covered.
		time_left -= time_left * pm.fraction;

		// Did we run out of planes to clip against?
		if (numplanes >= MAX_CLIP_PLANES)
		{
			// this shouldn't really happen
			//  Stop our movement if so.
			VectorCopy(vec3_origin, mv->m_vecVelocity);
			// Con_DPrintf("Too many planes 4\n");

			break;
		}

		// Set up next clipping plane
		VectorCopy(pm.planeNormal, planes[numplanes]);
		numplanes++;

		// modify original_velocity so it parallels all of the clip planes
		//

		// reflect player velocity
		// Only give this a try for first impact plane because you can get yourself stuck in an acute corner by
		// jumping in place
		//  and pressing forward and nobody was really using this bounce/reflection feature anyway...
		if (numplanes == 1 && player->m_MoveType() == MOVETYPE_WALK && player->m_hGroundEntity() == nullptr)
		{
			// Is this a floor/slope that the player can walk on?
			if (planes[0][2] >= 0.7)
			{
				ClipVelocity_Custom(original_velocity, planes[0], new_velocity, 1);
				VectorCopy(new_velocity, original_velocity);
			}
			else // either the player is surfing or slammed into a wall
			{
				ClipVelocity_Custom(original_velocity, planes[0], new_velocity, 1.0f);
			}

			VectorCopy(new_velocity, mv->m_vecVelocity);
			VectorCopy(new_velocity, original_velocity);
		}
		else
		{
			for (i = 0; i < numplanes; i++)
			{
				ClipVelocity_Custom(original_velocity, planes[i], mv->m_vecVelocity, 1.0);
				for (j = 0; j < numplanes; j++)
				{
					if (j != i)
					{
						// Are we now moving against this plane?
						if (mv->m_vecVelocity.Dot(planes[j]) < 0)
							break; // not ok
					}
				}

				if (j == numplanes) // Didn't have to clip, so we're ok
					break;
			}

			// Did we go all the way through plane set
			if (i != numplanes)
			{
				// go along this plane
				// pmove.velocity is set in clipping call, no need to set again.
			}
			else
			{ // go along the crease
				if (numplanes != 2)
				{
					VectorCopy(vec3_origin, mv->m_vecVelocity);
					break;
				}

				// Fun fact time: these next five lines of code fix (vertical) rampbug
				if (CloseEnough(planes[0], planes[1], FLT_EPSILON))
				{
					// Why did the above return true? Well, when surfing, you can "clip" into the
					// ramp, due to the ramp not pushing you away enough, and when that happens,
					// a surfer cries. So the game thinks the surfer is clipping along two of the exact
					// same planes. So what we do here is take the surfer's original velocity,
					// and add the along the normal of the surf ramp they're currently riding down,
					// essentially pushing them away from the ramp.

					// Note: Technically the 20.0 here can be 2.0, but that causes "jitters" sometimes, so I found
					// 20 to be pretty safe and smooth. If it causes any unforeseen consequences, tweak it!
					VectorMA(original_velocity, 20.0f, planes[0], new_velocity);
					mv->m_vecVelocity.x = new_velocity.x;
					mv->m_vecVelocity.y = new_velocity.y;
					// Note: We don't want the player to gain any Z boost/reduce from this, gravity should be the
					// only force working in the Z direction!

					// Lastly, let's get out of here before the following lines of code make the surfer lose speed.
					break;
				}

				// Though now it's good to note: the following code is needed for when a ramp creates a "V" shape,
				// and pinches the surfer between two planes of differing normals.
				CrossProduct(planes[0], planes[1], dir);
				dir.NormalizeInPlace();
				d = dir.Dot(mv->m_vecVelocity);
				VectorScale(dir, d, mv->m_vecVelocity);
			}

			//
			// if original velocity is against the original velocity, stop dead
			// to avoid tiny oscillations in sloping corners
			//
			d = mv->m_vecVelocity.Dot(primal_velocity);
			if (d <= 0)
			{
				// Con_DPrintf("Back\n");
				VectorCopy(vec3_origin, mv->m_vecVelocity);
				break;
			}
		}
	}

	if (CloseEnough(allFraction, 0.0f, FLT_EPSILON))
	{
		VectorCopy(vec3_origin, mv->m_vecVelocity);
	}
}
#endif


// CONVAR_TODO
bool g_bRampBugFixEnabled = false;

void FASTCALL Detour_CCSPlayer_MovementServices_TryPlayerMove(CCSPlayer_MovementServices *ms, CMoveData *mv, Vector *pFirstDest, trace_t_s2 *pFirstTrace)
{
	if (g_bRampBugFixEnabled)
		TryPlayerMove_Custom(ms, mv, pFirstDest, pFirstTrace);
	else
		CCSPlayer_MovementServices_TryPlayerMove(ms, mv, pFirstDest, pFirstTrace);
}

CON_COMMAND_F(cs2f_rampbugfix_enable, "Whether to enable rampbugfix", FCVAR_LINKED_CONCOMMAND | FCVAR_SPONLY)
{
	if (args.ArgC() < 2)
		Msg("%s %i\n", args[0], g_bRampBugFixEnabled);
	else
		g_bRampBugFixEnabled = V_StringToBool(args[1], false);
}

CUtlVector<CDetourBase *> g_vecDetours;

bool InitDetours(CGameConfig *gameConfig)
{
	bool success = true;

	g_vecDetours.PurgeAndDeleteElements();

	for (int i = 0; i < sizeof(g_LoggingDetours) / sizeof(*g_LoggingDetours); i++)
	{
		if (!g_LoggingDetours[i].CreateDetour(gameConfig))
			success = false;
	}

	if (!UTIL_SayTextFilter.CreateDetour(gameConfig))
		success = false;
	UTIL_SayTextFilter.EnableDetour();

	if (!UTIL_SayText2Filter.CreateDetour(gameConfig))
		success = false;
	UTIL_SayText2Filter.EnableDetour();

	if (!IsHearingClient.CreateDetour(gameConfig))
		success = false;
	IsHearingClient.EnableDetour();

	if (!CSoundEmitterSystem_EmitSound.CreateDetour(gameConfig))
		success = false;
	CSoundEmitterSystem_EmitSound.EnableDetour();

	if (!CCSWeaponBase_Spawn.CreateDetour(gameConfig))
		success = false;
	CCSWeaponBase_Spawn.EnableDetour();

	if (!TriggerPush_Touch.CreateDetour(gameConfig))
		success = false;
	TriggerPush_Touch.EnableDetour();

	if (!CGameRules_Constructor.CreateDetour(gameConfig))
		success = false;
	CGameRules_Constructor.EnableDetour();

	if (!CBaseEntity_TakeDamageOld.CreateDetour(gameConfig))
		success = false;
	CBaseEntity_TakeDamageOld.EnableDetour();

	if (!CNavMesh_GetNearestNavArea.CreateDetour(gameConfig))
		success = false;
	CNavMesh_GetNearestNavArea.EnableDetour();

	if (!CCSPlayer_MovementServices_TryPlayerMove.CreateDetour(gameConfig))
		success = false;
	CCSPlayer_MovementServices_TryPlayerMove.EnableDetour();

	return success;
}

void FlushAllDetours()
{
	g_vecDetours.Purge();
}
