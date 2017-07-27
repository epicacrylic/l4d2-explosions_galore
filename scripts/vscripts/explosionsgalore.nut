MutationOptions <-
{
	CommonLimit = 0 // Maximum number of common zombies alive in the world at the same time
	MegaMobSize = 0 // Total number of common zombies in a mob. (never more than CommonLimit at one time)
	WanderingZombieDensityModifier = 0 // lets get rid of the wandering zombies
	MaxSpecials  = 18
	TankLimit    = 18
	WitchLimit   = 0
	BoomerLimit  = 0
	ChargerLimit = 0
	HunterLimit  = 0
	JockeyLimit  = 0
	SpitterLimit = 0
	SmokerLimit  = 0
}

MutationState <-
{
	// make vars here
	TrTimer = 0
	TrAddTimer = 0
	TrNeedFinaleFix = false
	TrNeedRespawnTanks = false
	TrTankRespawnAmount = 0
	TrTankKilledAmount = 0
	TrHasFinale = false
	TrTanksToSpawn = 0
}

TankSpawnPoint <- {
    type = 8
    pos = null
    targetname = "rushtank"
}

DirectorOptions <-
{
	ActiveChallenge = 1
	
	cm_SpecialSlotCountdownTime = 1
	cm_SpecialRespawnInterval = 1
	ShouldAllowSpecialsWithTank  = true
	cm_ShouldAllowSpecialsWithTank  = true

	weaponsToRemove =
	{
		weapon_pistol = 0
		weapon_pistol_magnum = 0
		weapon_smg = 0
		weapon_pumpshotgun = 0
		weapon_autoshotgun = 0
		weapon_rifle = 0
		weapon_hunting_rifle = 0
		weapon_smg_silenced = 0
		weapon_shotgun_chrome = 0
		weapon_rifle_desert = 0
		weapon_sniper_military = 0
		weapon_shotgun_spas = 0
		weapon_rifle_ak47 = 0
		weapon_smg_mp5 = 0		
		weapon_rifle_sg552 = 0		
		weapon_sniper_awp = 0	
		weapon_sniper_scout = 0
		weapon_rifle_m60 = 0
		weapon_melee = 0
		weapon_chainsaw = 0
		weapon_rifle_m60 = 0
		weapon_grenade_launcher = 0
	}

	function AllowWeaponSpawn( classname )
	{
		if ( classname in weaponsToRemove )
		{
			return false;
		}
		return true;
	}	

	DefaultItems =
	[
		"weapon_grenade_launcher",
		"weapon_pistol_magnum",
	]

	function GetDefaultItem( idx )
	{
		if ( idx < DefaultItems.len() )
		{
			return DefaultItems[idx];
		}
		return 0;
	}	
}

function OnGameplayStart()
{
	SessionState.TrTimer = 0
	SessionState.TrNeedFinaleFix = false
	SessionState.TrNeedRespawnTanks = false
	SessionState.TrTankRespawnAmount = 0
	SessionState.TrTankKilledAmount = 0
	SessionState.TrTanksToSpawn = 0
	
	if(Entities.FindByClassname(null, "trigger_finale") || Entities.FindByName(null, "finale") || Entities.FindByName(null, "finale_template") || Entities.FindByName(null, "finale_fake") || Entities.FindByName(null, "ptemplate_finale"))
	{
		SessionState.TrHasFinale = true
		SessionOptions.BoomerLimit = 10
	}
	else
	{
		SessionState.TrHasFinale = false
		SessionOptions.BoomerLimit = 0
	}
	//printl("Game start!")
}

function Update()
{
	//printl("Update! DinG!!");
	
	//printl(SessionState.TrHasFinale)
	
	if (Director.HasAnySurvivorLeftSafeArea())
	{
		if( Time() - SessionState.TrTimer >= 12 )
		{
			SessionState.TrTimer = Time();
			if(SessionState.TrNeedRespawnTanks)
			{
				SessionState.TrTankKilledAmount = 0
				if(SessionState.TrHasFinale)
				{
					SessionState.TrTanksToSpawn = SessionState.TrTankRespawnAmount
					SessionState.TrTankRespawnAmount = 0
				}
				else
				{
					EntFire( "info_director", "scriptedpanicevent", "tankspawner" );
					SessionState.TrTankRespawnAmount--
				}
				if(SessionState.TrTankRespawnAmount <= 0)
				{
					SessionState.TrNeedRespawnTanks = false
				}
			}
			else
			{
				if(SessionState.TrHasFinale)
				{
					SessionState.TrTanksToSpawn++
				}
				else
				{
					EntFire( "info_director", "scriptedpanicevent", "tankspawner" );
				}
			}
			//printl("Spawning a tank");
		}
	}
	else
	{
		SessionState.TrTimer = 0;
	}
}

function OnGameEvent_tank_killed( params )
{
    local playerEnt = null;
	
	while ( playerEnt = Entities.FindByClassname( playerEnt, "player" ) )
	{
		if ( playerEnt.IsSurvivor() && !playerEnt.IsDead() )
		{
			playerEnt.ReviveFromIncap();
			playerEnt.SetHealth(100);
			playerEnt.SetHealthBuffer(0);
			playerEnt.SetReviveCount(0);
		}
	}
	
	if(SessionState.TrNeedFinaleFix)
	{
		SessionState.TrTankKilledAmount++
		
		//printl("Tanks killed so far: " + SessionState.TrTankKilledAmount)
		
		if(SessionState.TrTankKilledAmount == 15)
		{
			//printl("KILLING TANKS!!!")
		
			SessionState.TrTankKilledAmount = 0
			SessionState.TrNeedRespawnTanks = true
			SessionState.TrTimer = Time() - 2
			
			while ( playerEnt = Entities.FindByClassname( playerEnt, "player" ) )
			{
				if ( !playerEnt.IsSurvivor() && playerEnt.GetZombieType() == 8 && !playerEnt.IsDead())
				{
					SessionState.TrTankRespawnAmount++
					
					//playerEnt.Kill()
					
					DoEntFire("!self","sethealth","0",0,null,playerEnt);
				}
			}
	
		}
	}
}

function OnGameEvent_player_spawn(event) 
{
	local ent = GetPlayerFromUserID(event["userid"]);

	if (ent.GetZombieType() != 9 && ent.GetZombieType() != 8) 
	{
		//printl("nontank spawn")
		ent.Kill()
		
		if(SessionState.TrTanksToSpawn > 0)
		{
			local pos = ent.GetOrigin()
			TankSpawnPoint.pos = pos
			ZSpawn(TankSpawnPoint)
		}
	}
	else if(ent.GetZombieType() == 8)
	{
		//printl("tank spawn")
		if(SessionState.TrTanksToSpawn > 0)
		{
			SessionState.TrTanksToSpawn--
		}
	}
}

function OnGameEvent_tank_spawn( params )
{
	Ent(params.tankid).SetHealth(4000)
}

function OnGameEvent_finale_start( params )
{
	if(!Entities.FindByClassname(null,"game_scavenge_progress_display"))
	{
		SessionState.TrNeedFinaleFix = true;
	}
	
	if(!Entities.FindByModel(null, "models/infected/hulk.mdl"))
	{
		SessionState.TrTimer = Time();
		if(SessionState.TrHasFinale)
		{
			SessionState.TrTanksToSpawn++
		}
		else
		{
			EntFire( "info_director", "scriptedpanicevent", "tankspawner" );
		}
	}
	SessionState.TrTanksToSpawn = 1
	SessionState.TrHasFinale = true
}

function OnGameEvent_gauntlet_finale_start( params )
{
	if(!Entities.FindByModel(null, "models/infected/hulk.mdl"))
	{
		SessionState.TrTimer = Time();
		if(SessionState.TrHasFinale)
		{
			SessionState.TrTanksToSpawn++
		}
		else
		{
			EntFire( "info_director", "scriptedpanicevent", "tankspawner" );
		}
	}
	SessionState.TrTanksToSpawn = 1
	SessionState.TrHasFinale = true
}